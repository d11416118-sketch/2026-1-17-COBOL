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
        
        # 修改：users 資料表增加 created_at
        c.execute('''CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, phone TEXT UNIQUE, created_at TEXT)''')
        
        # 檢查 users 表是否有 created_at 欄位 (為了相容舊資料庫)
        c.execute("PRAGMA table_info(users)")
        columns = [info[1] for info in c.fetchall()]
        if 'created_at' not in columns:
            print("⚙️ 正在升級資料庫：新增 users.created_at 欄位...")
            c.execute("ALTER TABLE users ADD COLUMN created_at TEXT")

        c.execute("SELECT count(*) FROM products")
        if c.fetchone()[0] == 0:
            default_menu = [('紅茶', 30, 'black_tea.png'), ('奶茶', 40, 'milk_tea.png'), ('綠茶', 30, 'green_tea.png'), ('烏龍', 30, 'oolong.png'), ('拿鐵', 60, 'latte.png')]
            c.executemany("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", default_menu)

def get_byte_aligned_str(text, width):
    b_text = str(text).encode('utf-8')
    if len(b_text) > width:
        b_text = b_text[:width]
    s_text = b_text.decode('utf-8', errors='ignore')
    real_len = len(s_text.encode('utf-8'))
    pad_len = width - real_len
    return s_text + (' ' * pad_len)

# 關鍵功能：更新 COBOL 用的 orders.txt
def export_orders_to_cobol():
    try:
        with sqlite3.connect('shop.db') as conn:
            c = conn.cursor()
            # 確保只撈取未被刪除的訂單 (如果我們不做軟刪除，直接從 DB 撈就是最新的)
            c.execute("SELECT name, phone, item, qty, price FROM orders")
            rows = c.fetchall()
        
        with open('orders.txt', 'w', encoding='utf-8') as f:
            for r in rows:
                name = get_byte_aligned_str(r[0], 20)
                phone = get_byte_aligned_str(r[1], 15)
                item = get_byte_aligned_str(r[2], 10)
                qty = int(r[3])
                total_price = int(r[4])
                unit_price = total_price // qty if qty > 0 else 0
                f.write(f"{name}{phone}{item}{unit_price:03d}{qty:03d}{total_price:05d}\n")
        print("✅ COBOL 資料檔 (orders.txt) 更新成功！(營收與排行將隨之變更)")
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
    
    export_orders_to_cobol() 
    return jsonify({"status": "success", "receipt_html": "<br>".join(receipt_lines), "total": 0, "time": current_time})

# --- 管理功能區 ---

# 刪除待製作訂單
@app.route('/api/orders/<int:oid>', methods=['DELETE'])
def delete_order(oid):
    with sqlite3.connect('shop.db') as conn:
        conn.execute("DELETE FROM orders WHERE id = ?", (oid,))
    export_orders_to_cobol() # 更新 COBOL
    return jsonify({"status": "success"})

# 刪除打卡紀錄
@app.route('/api/attendance/<int:aid>', methods=['DELETE'])
def delete_attendance(aid):
    with sqlite3.connect('shop.db') as conn:
        conn.execute("DELETE FROM attendance WHERE id = ?", (aid,))
    return jsonify({"status": "success"})

# 新增功能：刪除歷史訂單 (連動更新 COBOL orders.txt)
@app.route('/api/history/<int:hid>', methods=['DELETE'])
def delete_history(hid):
    with sqlite3.connect('shop.db') as conn:
        conn.execute("DELETE FROM orders WHERE id = ?", (hid,))
    export_orders_to_cobol() # 關鍵：這邊一刪除，orders.txt 就會變少，下次跑 view.exe 營收就會扣除
    return jsonify({"status": "success"})

# 新增功能：會員管理 (取得列表)
@app.route('/api/members', methods=['GET'])
def get_members():
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT id, username, phone, created_at FROM users ORDER BY id DESC")
        members = [{"id": r[0], "name": r[1], "phone": r[2], "time": r[3] or "未知"} for r in c.fetchall()]
    return jsonify(members)

# 新增功能：刪除會員
@app.route('/api/members/<int:mid>', methods=['DELETE'])
def delete_member(mid):
    with sqlite3.connect('shop.db') as conn:
        conn.execute("DELETE FROM users WHERE id = ?", (mid,))
    return jsonify({"status": "success"})

@app.route('/api/history_search')
def history_search():
    start_date = request.args.get('start') 
    end_date = request.args.get('end')
    
    if not start_date or not end_date: return jsonify([])

    start_str = f"{start_date} 00:00:00"
    end_str = f"{end_date} 23:59:59"

    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("""
            SELECT id, name, phone, item, qty, price, options, created_at 
            FROM orders 
            WHERE status = 'completed' AND created_at BETWEEN ? AND ?
            ORDER BY created_at DESC
        """, (start_str, end_str))
        history = [{"id":r[0], "name":r[1], "phone":r[2], "item":r[3], "qty":r[4], "price":r[5], "options":r[6], "time":r[7]} for r in c.fetchall()]
    return jsonify(history)

@app.route('/api/admin/data')
def get_admin_data():
    try:
        with sqlite3.connect('shop.db') as conn:
            c = conn.cursor()
            # 待製作
            c.execute("SELECT id, name, phone, item, qty, price, options, created_at FROM orders WHERE status = 'pending'")
            pending = [{"id":r[0], "name":r[1], "phone":r[2], "item":r[3], "qty":r[4], "price":r[5], "options":r[6], "time":r[7]} for r in c.fetchall()]
            
            # 預設近期完成 (顯示最近10筆)
            c.execute("SELECT id, name, phone, item, qty, price, options, created_at FROM orders WHERE status = 'completed' ORDER BY id DESC LIMIT 10")
            completed = [{"id":r[0], "name":r[1], "phone":r[2], "item":r[3], "qty":r[4], "price":r[5], "options":r[6], "time":r[7]} for r in c.fetchall()]
            
            # 打卡紀錄
            c.execute("SELECT id, staff_name, action_type, log_time FROM attendance ORDER BY id DESC LIMIT 10")
            attendance = [{"id": r[0], "name":r[1], "action":r[2], "time":r[3]} for r in c.fetchall()]
            
            return jsonify({"pending": pending, "completed": completed, "attendance": attendance})
    except Exception as e:
        print(f"後台錯誤: {e}")
        return jsonify({"pending": [], "completed": [], "attendance": []})

@app.route('/api/cobol_report')
def get_cobol_report():
    report_path = 'report.txt'
    data = {"status": "Waiting", "revenue": 0, "popular": "N/A", "audit": "Pending"}
    if os.path.exists(report_path):
        try:
            with open(report_path, 'r', encoding='utf-8') as f:
                for line in f:
                    if '=' in line:
                        key, value = line.strip().split('=', 1)
                        if key == "REVENUE": data["revenue"] = int(value)
                        elif key == "POPULAR": data["popular"] = value
                        elif key == "AUDIT": data["audit"] = value
            data["status"] = "Ready"
        except Exception as e:
            print(f"讀取報告失敗: {e}")
    return jsonify(data)        

@app.route('/api/complete', methods=['POST'])
def complete_order():
    d = request.json
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
        # 修改：註冊時寫入 created_at
        current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        with sqlite3.connect('shop.db') as conn: 
            conn.execute("INSERT INTO users (username, password, phone, created_at) VALUES (?, ?, ?, ?)", 
                        (d.get('username'), generate_password_hash(d.get('password')), d.get('phone'), current_time))
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