import sqlite3
import os
import random  # 新增：用來產生亂數
from flask import Flask, request, jsonify, send_file, session
from datetime import datetime
from werkzeug.utils import secure_filename
from werkzeug.security import generate_password_hash, check_password_hash

app = Flask(__name__)
app.secret_key = 'super_secret_key_for_session'

UPLOAD_FOLDER = 'static'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
ALLOWED_EXTENSIONS = {'png', 'jpg', 'jpeg', 'gif'}

# 用來暫存驗證碼的字典 { "0912345678": "123456" }
# 注意：重啟程式後這裡會清空
otp_storage = {}

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

def init_db():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    c.execute('''CREATE TABLE IF NOT EXISTS orders (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, phone TEXT, item TEXT, qty INTEGER, price INTEGER, created_at TEXT, options TEXT, status TEXT DEFAULT 'pending')''')
    c.execute('''CREATE TABLE IF NOT EXISTS attendance (id INTEGER PRIMARY KEY AUTOINCREMENT, staff_name TEXT, action_type TEXT, log_time TEXT)''')
    c.execute('''CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, price INTEGER, image TEXT)''')
    c.execute('''CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, phone TEXT UNIQUE)''')
    
    c.execute("SELECT count(*) FROM products")
    if c.fetchone()[0] == 0:
        default_menu = [('紅茶', 30, 'black_tea.png'), ('奶茶', 40, 'milk_tea.png'), ('綠茶', 30, 'green_tea.png'), ('烏龍', 30, 'oolong.png'), ('拿鐵', 60, 'latte.png')]
        c.executemany("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", default_menu)
        conn.commit()
    conn.commit()
    conn.close()

init_db()

@app.route('/')
def customer_ui(): return send_file('index.html')
@app.route('/admin')
def staff_ui(): return send_file('admin.html')

# --- 驗證碼相關 API (新功能) ---

@app.route('/api/send_otp', methods=['POST'])
def send_otp():
    phone = request.json.get('phone')
    if not phone:
        return jsonify({"status": "error", "msg": "請輸入手機號碼"})
    
    # 1. 產生 6 位數亂數
    code = str(random.randint(100000, 999999))
    
    # 2. 存入暫存區 (真實專案會存 Redis，這裡存記憶體即可)
    otp_storage[phone] = code
    
    # 3. 【關鍵】模擬發送：直接印在伺服器黑畫面
    print("="*30)
    print(f"【簡訊模擬】給 {phone} 的驗證碼是: {code}")
    print("="*30)
    
    return jsonify({"status": "success", "msg": "驗證碼已發送 (請看終端機)"})

# ----------------------------

@app.route('/api/status')
def check_status():
    if 'user_phone' in session:
        return jsonify({"logged_in": True, "username": session['user_name'], "phone": session['user_phone']})
    return jsonify({"logged_in": False})

@app.route('/api/register', methods=['POST'])
def register():
    data = request.json
    username = data.get('username')
    password = data.get('password')
    phone = data.get('phone')
    otp_input = data.get('otp')  # 使用者輸入的驗證碼

    if not all([username, password, phone, otp_input]):
        return jsonify({"status": "error", "msg": "所有欄位都必填"})

    # 4. 檢查驗證碼是否正確
    if otp_storage.get(phone) != otp_input:
        return jsonify({"status": "error", "msg": "驗證碼錯誤或已過期"})

    hashed_pw = generate_password_hash(password)
    try:
        with sqlite3.connect('shop.db') as conn:
            conn.execute("INSERT INTO users (username, password, phone) VALUES (?, ?, ?)", (username, hashed_pw, phone))
        # 註冊成功後，清除該驗證碼
        del otp_storage[phone]
        return jsonify({"status": "success"})
    except sqlite3.IntegrityError:
        return jsonify({"status": "error", "msg": "此手機號碼已被註冊"})

@app.route('/api/login', methods=['POST'])
def login():
    data = request.json
    phone = data.get('phone')
    password = data.get('password')
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT username, password, phone FROM users WHERE phone = ?", (phone,))
        user = c.fetchone()
    if user and check_password_hash(user[1], password):
        session['user_name'] = user[0]
        session['user_phone'] = user[2]
        return jsonify({"status": "success", "username": user[0]})
    else:
        return jsonify({"status": "error", "msg": "帳號或密碼錯誤"})

@app.route('/api/logout')
def logout():
    session.clear()
    return jsonify({"status": "success"})

@app.route('/api/my_history')
def my_history():
    if 'user_phone' not in session: return jsonify({"status": "error", "msg": "請先登入"})
    phone = session['user_phone']
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT created_at, item, options, qty, price, status FROM orders WHERE phone = ? ORDER BY id DESC", (phone,))
        rows = c.fetchall()
    return jsonify([{"time":r[0], "item":r[1], "options":r[2], "qty":r[3], "price":r[4], "status":r[5]} for r in rows])

# --- 以下維持原樣 (Products, Checkout, Admin, Clockin) ---

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
    return jsonify({"status": "error", "msg": "上傳失敗"})

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
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    receipt_lines = []
    for item in cart_items:
        total = int(item['price']) * int(item['qty'])
        c.execute("INSERT INTO orders (name, phone, item, qty, price, created_at, options, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)", (c_name, c_phone, item['name'], int(item['qty']), total, current_time, f"({item['sugar']}/{item['ice']})", 'pending'))
        receipt_lines.append(f"{item['name']} x{item['qty']} - ${total}")
    conn.commit()
    conn.close()
    return jsonify({"status": "success", "receipt_html": "<br>".join(receipt_lines), "total": 0, "time": current_time})

@app.route('/api/admin/data', methods=['GET'])
def get_admin_data():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    c.execute("SELECT name, phone, item, qty, price, created_at, options, status FROM orders ORDER BY id DESC")
    o_rows = c.fetchall()
    c.execute("SELECT staff_name, action_type, log_time FROM attendance ORDER BY id DESC LIMIT 10")
    c_rows = c.fetchall()
    conn.close()
    p_list, c_list = {}, {}
    for r in o_rows:
        key = (r[1], r[5])
        target = p_list if r[7] == 'pending' else c_list
        item_str = f"{r[2]} {r[6]} x{r[3]}"
        if key not in target: target[key] = {"name": r[0], "phone": r[1], "items": [item_str], "price": r[4], "time": r[5]}
        else: target[key]["items"].append(item_str); target[key]["price"] += r[4]
    return jsonify({"pending": list(p_list.values()), "completed": list(c_list.values()), "attendance": [{"name":r[0], "action":r[1], "time":r[2]} for r in c_rows]})

@app.route('/api/complete', methods=['POST'])
def complete_order():
    d = request.json
    with sqlite3.connect('shop.db') as conn: conn.execute("UPDATE orders SET status = 'completed' WHERE phone = ? AND created_at = ?", (d.get('phone'), d.get('time')))
    return jsonify({"status": "success"})

@app.route('/api/clockin', methods=['POST'])
def clock_in():
    d = request.json
    with sqlite3.connect('shop.db') as conn: conn.execute("INSERT INTO attendance (staff_name, action_type, log_time) VALUES (?, ?, ?)", (d.get('name'), d.get('action'), datetime.now().strftime("%Y-%m-%d %H:%M:%S")))
    return jsonify({"status": "success"})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)