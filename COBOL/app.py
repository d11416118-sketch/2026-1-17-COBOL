import sqlite3
import os
from flask import Flask, request, jsonify, send_file
from datetime import datetime
from werkzeug.utils import secure_filename

app = Flask(__name__)

# 設定圖片上傳存檔的位置
UPLOAD_FOLDER = 'static'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
ALLOWED_EXTENSIONS = {'png', 'jpg', 'jpeg', 'gif'}

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

# 初始化資料庫 (含商品表與預設資料)
def init_db():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    
    # 1. 訂單表
    c.execute('''CREATE TABLE IF NOT EXISTS orders
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  name TEXT, phone TEXT, item TEXT, qty INTEGER, price INTEGER, 
                  created_at TEXT, options TEXT, status TEXT DEFAULT 'pending')''')
    
    # 2. 打卡表
    c.execute('''CREATE TABLE IF NOT EXISTS attendance
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  staff_name TEXT, action_type TEXT, log_time TEXT)''')

    # 3. 商品表 (新功能)
    c.execute('''CREATE TABLE IF NOT EXISTS products
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  name TEXT, price INTEGER, image TEXT)''')
    
    # 檢查是否為空的，如果是空的就塞入預設菜單
    c.execute("SELECT count(*) FROM products")
    if c.fetchone()[0] == 0:
        default_menu = [
            ('紅茶', 30, 'black_tea.png'),
            ('奶茶', 40, 'milk_tea.png'),
            ('綠茶', 30, 'green_tea.png'),
            ('烏龍', 30, 'oolong.png'),
            ('拿鐵', 60, 'latte.png')
        ]
        c.executemany("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", default_menu)
        conn.commit()
        print("已初始化預設菜單")

    conn.commit()
    conn.close()

init_db()

@app.route('/')
def customer_ui():
    return send_file('index.html')

@app.route('/admin')
def staff_ui():
    return send_file('admin.html')

# --- 商品管理 API (新功能) ---

@app.route('/api/products', methods=['GET'])
def get_products():
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT id, name, price, image FROM products")
        products = [{"id": row[0], "name": row[1], "price": row[2], "image": row[3]} for row in c.fetchall()]
    return jsonify(products)

@app.route('/api/products', methods=['POST'])
def add_product():
    # 接收文字資料
    name = request.form.get('name')
    price = request.form.get('price')
    
    # 接收圖片檔案
    if 'image' not in request.files:
        return jsonify({"status": "error", "msg": "沒有上傳圖片"})
    
    file = request.files['image']
    if file.filename == '':
        return jsonify({"status": "error", "msg": "未選擇檔案"})

    if file and allowed_file(file.filename):
        filename = secure_filename(file.filename)
        file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
        
        with sqlite3.connect('shop.db') as conn:
            conn.execute("INSERT INTO products (name, price, image) VALUES (?, ?, ?)", (name, price, filename))
        return jsonify({"status": "success"})
    
    return jsonify({"status": "error", "msg": "檔案格式不支援"})

@app.route('/api/products/<int:p_id>', methods=['DELETE'])
def delete_product(p_id):
    with sqlite3.connect('shop.db') as conn:
        conn.execute("DELETE FROM products WHERE id = ?", (p_id,))
    return jsonify({"status": "success"})

# --- 原有的 API ---

@app.route('/api/checkout', methods=['POST'])
def checkout():
    data = request.json
    c_name = data.get('customer_name')
    c_phone = data.get('customer_phone')
    cart_items = data.get('items')
    current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    receipt_lines = []
    
    for item in cart_items:
        # 注意：這裡不再從後端驗證單價，簡化流程，直接信任前端傳來的價格
        # 實務上應該要拿 item ID 去資料庫查價格才安全
        c.execute("INSERT INTO orders (name, phone, item, qty, price, created_at, options, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                  (c_name, c_phone, item['name'], int(item['qty']), int(item['price'])*int(item['qty']), current_time, f"({item['sugar']}/{item['ice']})", 'pending'))
        receipt_lines.append(f"{item['name']} x{item['qty']} - ${int(item['price'])*int(item['qty'])}")

    conn.commit()
    conn.close()
    return jsonify({"status": "success", "receipt_html": "<br>".join(receipt_lines), "total": 0, "time": current_time})

@app.route('/api/admin/data', methods=['GET'])
def get_admin_data():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    c.execute("SELECT name, phone, item, qty, price, created_at, options, status FROM orders ORDER BY id DESC")
    order_rows = c.fetchall()
    c.execute("SELECT staff_name, action_type, log_time FROM attendance ORDER BY id DESC LIMIT 10")
    clock_rows = c.fetchall()
    conn.close()
    
    pending_list = {}
    completed_list = {}
    for row in order_rows:
        key = (row[1], row[5])
        target = pending_list if row[7] == 'pending' else completed_list
        item_str = f"{row[2]} {row[6]} x{row[3]}"
        if key not in target:
            target[key] = {"name": row[0], "phone": row[1], "items": [item_str], "price": row[4], "time": row[5]}
        else:
            target[key]["items"].append(item_str)
            target[key]["price"] += row[4]

    return jsonify({"pending": list(pending_list.values()), "completed": list(completed_list.values()), "attendance": [{"name":r[0], "action":r[1], "time":r[2]} for r in clock_rows]})

@app.route('/api/complete', methods=['POST'])
def complete_order():
    d = request.json
    with sqlite3.connect('shop.db') as conn:
        conn.execute("UPDATE orders SET status = 'completed' WHERE phone = ? AND created_at = ?", (d.get('phone'), d.get('time')))
    return jsonify({"status": "success"})

@app.route('/api/clockin', methods=['POST'])
def clock_in():
    d = request.json
    with sqlite3.connect('shop.db') as conn:
        conn.execute("INSERT INTO attendance (staff_name, action_type, log_time) VALUES (?, ?, ?)", 
                     (d.get('name'), d.get('action'), datetime.now().strftime("%Y-%m-%d %H:%M:%S")))
    return jsonify({"status": "success"})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)