import sqlite3
import os
import random
from flask import Flask, request, jsonify, send_file, session
from datetime import datetime
from werkzeug.utils import secure_filename
from werkzeug.security import generate_password_hash, check_password_hash

app = Flask(__name__)
app.secret_key = 'super_secret_key_for_session'

UPLOAD_FOLDER = 'static'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
ALLOWED_EXTENSIONS = {'png', 'jpg', 'jpeg', 'gif'}
otp_storage = {}

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

def init_db():
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute('''CREATE TABLE IF NOT EXISTS orders (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, phone TEXT, item TEXT, qty INTEGER, price INTEGER, created_at TEXT, options TEXT, status TEXT DEFAULT 'pending')''')
        c.execute('''CREATE TABLE IF NOT EXISTS attendance (id INTEGER PRIMARY KEY AUTOINCREMENT, staff_name TEXT, action_type TEXT, log_time TEXT)''')
        c.execute('''CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, price INTEGER, image TEXT)''')
        c.execute('''CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, phone TEXT UNIQUE)''')
        
        c.execute("SELECT count(*) FROM products")
        if c.fetchone()[0] == 0:
            default_menu = [('紅茶', 30, 'black_tea.png'), ('奶茶', 40, 'milk_tea.png'), ('綠茶', 30, 'green_tea.png'), ('烏龍', 30, 'oolong.png'), ('拿鐵', 60, 'latte.png')]
            c.executemany("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", default_menu)

# 修改 app.py 中的 helper 函式

def get_byte_aligned_str(text, width):
    """
    黑魔法函式 (最終修正版)：
    確保字串被截斷後，寫入檔案的總 byte 數依然精準等於 width
    """
    # 1. 先轉成 bytes
    b_text = str(text).encode('utf-8')
    
    # 2. 如果太長就切掉
    if len(b_text) > width:
        b_text = b_text[:width]
        
    # 3. 轉回字串 (這一步會把切爛的最後一個 byte 丟掉)
    s_text = b_text.decode('utf-8', errors='ignore')
    
    # 4. ★★★ 關鍵修正 ★★★
    # 重新計算「轉回字串後」的實際有效 byte 長度
    # 例如 "文山包" 是 9 bytes (原本切成10 bytes，但第10個爛掉了被丟棄)
    real_len = len(s_text.encode('utf-8'))
    
    # 5. 算出要補多少空白 (width - 實際長度)
    # 10 - 9 = 1，所以要補 1 個空白，補回那個被丟掉的 byte
    pad_len = width - real_len
    
    return s_text + (' ' * pad_len)
def export_orders_to_cobol():
    try:
        with sqlite3.connect('shop.db') as conn:
            c = conn.cursor()
            c.execute("SELECT name, phone, item, qty, price FROM orders")
            rows = c.fetchall()
        
        with open('orders.txt', 'w', encoding='utf-8') as f:
            for r in rows:
                name = get_byte_aligned_str(r[0], 20)
                phone = get_byte_aligned_str(r[1], 15)
                item = get_byte_aligned_str(r[2], 10)
                f.write(f"{name}{phone}{item}{int(r[3]):03d}{int(r[4]):05d}\n")
        print("✅ COBOL 資料檔 (orders.txt) 更新成功！")
    except Exception as e:
        print(f"❌ COBOL 匯出失敗: {e}")

init_db()

# --- 路由區 ---
@app.route('/')
def customer_ui(): return send_file('index.html')

@app.route('/admin')
def staff_ui(): return send_file('admin.html')

@app.route('/api/products', methods=['GET'])
def get_products():
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT id, name, price, image FROM products")
        return jsonify([{"id":r[0], "name":r[1], "price":r[2], "image":r[3]} for r in c.fetchall()])

@app.route('/api/products', methods=['POST'])
def add_product():
    name = request.form.get('name')
    price = request.form.get('price')
    file = request.files.get('image')
    if file and allowed_file(file.filename):
        filename = secure_filename(file.filename)
        file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
        with sqlite3.connect('shop.db') as conn:
            conn.execute("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", (name, price, filename))
        return jsonify({"status": "success"})
    return jsonify({"status": "error"})

@app.route('/api/products/<int:p_id>', methods=['DELETE'])
def delete_product(p_id):
    with sqlite3.connect('shop.db') as conn: conn.execute("DELETE FROM products WHERE id = ?", (p_id,))
    return jsonify({"status": "success"})

@app.route('/api/checkout', methods=['POST'])
def checkout():
    data = request.json
    c_name = session.get('user_name', data.get('customer_name'))
    c_phone = session.get('user_phone', data.get('customer_phone'))
    cart_items = data.get('items')
    current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT name, price FROM products")
        price_map = {row[0]: row[1] for row in c.fetchall()}
        
        receipt_lines = []
        for item in cart_items:
            real_price = price_map.get(item['name'], 0)
            total = int(real_price) * int(item['qty'])
            c.execute("INSERT INTO orders (name, phone, item, qty, price, created_at, options, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)", 
                      (c_name, c_phone, item['name'], int(item['qty']), total, current_time, f"({item['sugar']}/{item['ice']})", 'pending'))
            receipt_lines.append(f"{item['name']} x{item['qty']} - ${total}")
    
    export_orders_to_cobol() # 轉檔
    return jsonify({"status": "success", "receipt_html": "<br>".join(receipt_lines), "total": 0, "time": current_time})

@app.route('/api/admin/data')
def get_admin_data():
    try:
        with sqlite3.connect('shop.db') as conn:
            c = conn.cursor()
            # 待製作 (Pending)
            c.execute("SELECT id, name, phone, item, qty, price, options, created_at FROM orders WHERE status = 'pending'")
            pending = [{"id":r[0], "name":r[1], "phone":r[2], "item":r[3], "qty":r[4], "price":r[5], "options":r[6], "time":r[7]} for r in c.fetchall()]
            
            # 近期完成 (Completed)
            c.execute("SELECT id, name, phone, item, qty, price, options, created_at FROM orders WHERE status = 'completed' ORDER BY id DESC LIMIT 10")
            completed = [{"id":r[0], "name":r[1], "phone":r[2], "item":r[3], "qty":r[4], "price":r[5], "options":r[6], "time":r[7]} for r in c.fetchall()]
            
            # 打卡紀錄
            c.execute("SELECT staff_name, action_type, log_time FROM attendance ORDER BY id DESC LIMIT 10")
            attendance = [{"name":r[0], "action":r[1], "time":r[2]} for r in c.fetchall()]
            
            return jsonify({"pending": pending, "completed": completed, "attendance": attendance})
    except Exception as e:
        print(f"後台錯誤: {e}")
        return jsonify({"pending": [], "completed": [], "attendance": []})

@app.route('/api/complete', methods=['POST'])
def complete_order():
    d = request.json
    # 根據 手機 + 時間 + 品項 來完成訂單 (避免只完成其中一杯)
    with sqlite3.connect('shop.db') as conn: 
        conn.execute("UPDATE orders SET status = 'completed' WHERE phone = ? AND created_at = ?", (d.get('phone'), d.get('time')))
    return jsonify({"status": "success"})

@app.route('/api/clockin', methods=['POST'])
def clock_in():
    d = request.json
    with sqlite3.connect('shop.db') as conn: conn.execute("INSERT INTO attendance (staff_name, action_type, log_time) VALUES (?, ?, ?)", (d.get('name'), d.get('action'), datetime.now().strftime("%Y-%m-%d %H:%M:%S")))
    return jsonify({"status": "success"})

# --- 認證相關 API ---
@app.route('/api/send_otp', methods=['POST'])
def send_otp():
    phone = request.json.get('phone')
    code = str(random.randint(100000, 999999))
    otp_storage[phone] = code
    print(f"【簡訊模擬】給 {phone} 的驗證碼: {code}")
    return jsonify({"status": "success"})

@app.route('/api/register', methods=['POST'])
def register():
    d = request.json
    if otp_storage.get(d.get('phone')) != d.get('otp'): return jsonify({"status": "error", "msg": "驗證碼錯誤"})
    try:
        with sqlite3.connect('shop.db') as conn: conn.execute("INSERT INTO users (username, password, phone) VALUES (?, ?, ?)", (d.get('username'), generate_password_hash(d.get('password')), d.get('phone')))
        del otp_storage[d.get('phone')]
        return jsonify({"status": "success"})
    except: return jsonify({"status": "error", "msg": "重複註冊"})

@app.route('/api/login', methods=['POST'])
def login():
    d = request.json
    with sqlite3.connect('shop.db') as conn: user = conn.execute("SELECT username, password, phone FROM users WHERE phone = ?", (d.get('phone'),)).fetchone()
    if user and check_password_hash(user[1], d.get('password')):
        session['user_name'], session['user_phone'] = user[0], user[2]
        return jsonify({"status": "success", "username": user[0]})
    return jsonify({"status": "error"})

@app.route('/api/logout')
def logout(): session.clear(); return jsonify({"status": "success"})

@app.route('/api/status')
def check_status(): return jsonify({"logged_in": 'user_phone' in session, "username": session.get('user_name'), "phone": session.get('user_phone')})

@app.route('/api/my_history')
def my_history():
    if 'user_phone' not in session: return jsonify([])
    with sqlite3.connect('shop.db') as conn:
        rows = conn.execute("SELECT created_at, item, options, qty, price, status FROM orders WHERE phone = ? ORDER BY id DESC", (session['user_phone'],)).fetchall()
    return jsonify([{"time":r[0], "item":r[1], "options":r[2], "qty":r[3], "price":r[4], "status":r[5]} for r in rows])

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)