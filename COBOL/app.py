import sqlite3
from flask import Flask, request, jsonify, send_file
from datetime import datetime

app = Flask(__name__)

# 1. 初始化資料庫 (新增 attendance 資料表)
def init_db():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    
    # 訂單表
    c.execute('''CREATE TABLE IF NOT EXISTS orders
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  name TEXT, phone TEXT, item TEXT, qty INTEGER, price INTEGER, 
                  created_at TEXT, options TEXT, status TEXT DEFAULT 'pending')''')
    
    # --- New: 打卡紀錄表 ---
    c.execute('''CREATE TABLE IF NOT EXISTS attendance
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  staff_name TEXT,
                  action_type TEXT,  -- "上班" 或 "下班"
                  log_time TEXT)''')
                  
    conn.commit()
    conn.close()

init_db()

@app.route('/')
def customer_ui():
    return send_file('index.html')

@app.route('/admin')
def staff_ui():
    return send_file('admin.html')

# --- 原有的 API 區 ---

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
        d_name = item['name']
        d_qty = int(item['qty'])
        d_price_unit = int(item['price'])
        option_str = f"({item.get('sugar')}/{item.get('ice')})"
        total_price = d_price_unit * d_qty
        
        c.execute("INSERT INTO orders (name, phone, item, qty, price, created_at, options, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                  (c_name, c_phone, d_name, d_qty, total_price, current_time, option_str, 'pending'))
        receipt_lines.append(f"{d_name} {option_str} x{d_qty} - ${total_price}")

    conn.commit()
    conn.close()
    
    receipt_html = "<br>".join(receipt_lines)
    total_bill = sum(item['price'] * item['qty'] for item in cart_items)
    return jsonify({"status": "success", "receipt_html": receipt_html, "total": total_bill, "time": current_time})

@app.route('/api/admin/data', methods=['GET'])
def get_admin_data():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    
    # 撈訂單
    c.execute("SELECT name, phone, item, qty, price, created_at, options, status FROM orders ORDER BY id DESC")
    order_rows = c.fetchall()
    
    # --- New: 撈打卡紀錄 (只撈最近 10 筆) ---
    c.execute("SELECT staff_name, action_type, log_time FROM attendance ORDER BY id DESC LIMIT 10")
    clock_rows = c.fetchall()
    
    conn.close()
    
    # 處理訂單邏輯 (分組)
    pending_list = {}
    completed_list = {}
    for row in order_rows:
        key = (row[1], row[5])
        status = row[7]
        target = pending_list if status == 'pending' else completed_list
        opt = str(row[6]) if row[6] else ""
        item_str = f"{row[2]} {opt} x{row[3]}"
        
        if key not in target:
            target[key] = {"name": row[0], "phone": row[1], "items": [item_str], "price": row[4], "time": row[5]}
        else:
            target[key]["items"].append(item_str)
            target[key]["price"] += row[4]

    # 處理打卡邏輯
    attendance_list = [{"name": r[0], "action": r[1], "time": r[2]} for r in clock_rows]

    return jsonify({
        "pending": list(pending_list.values()),
        "completed": list(completed_list.values()),
        "attendance": attendance_list  # 回傳打卡資料
    })

@app.route('/api/complete', methods=['POST'])
def complete_order():
    data = request.json
    conn = sqlite3.connect('shop.db')
    conn.execute("UPDATE orders SET status = 'completed' WHERE phone = ? AND created_at = ?", (data.get('phone'), data.get('time')))
    conn.commit()
    conn.close()
    return jsonify({"status": "success"})

# --- New: 打卡 API ---
@app.route('/api/clockin', methods=['POST'])
def clock_in():
    data = request.json
    name = data.get('name')
    action = data.get('action') # "上班" or "下班"
    time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    if not name: return jsonify({"status": "error", "msg": "請輸入姓名"})
    
    conn = sqlite3.connect('shop.db')
    conn.execute("INSERT INTO attendance (staff_name, action_type, log_time) VALUES (?, ?, ?)", (name, action, time))
    conn.commit()
    conn.close()
    
    return jsonify({"status": "success"})

if __name__ == '__main__':
    # host='0.0.0.0' 代表允許所有來源的連線
    app.run(host='0.0.0.0', port=5000, debug=True)