import sqlite3
from flask import Flask, request, jsonify, send_file
from datetime import datetime

app = Flask(__name__)

# 1. 初始化資料庫 (新增 status 欄位)
def init_db():
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    c.execute('''CREATE TABLE IF NOT EXISTS orders
                 (id INTEGER PRIMARY KEY AUTOINCREMENT,
                  name TEXT,
                  phone TEXT,
                  item TEXT,
                  qty INTEGER,
                  price INTEGER,
                  created_at TEXT,
                  options TEXT,
                  status TEXT DEFAULT 'pending')''') # status: pending(未做) / completed(已做)
    conn.commit()
    conn.close()

init_db()

# --- 路由區 ---
@app.route('/')
def customer_ui():
    return send_file('index.html') # 客戶端

@app.route('/admin')
def staff_ui():
    return send_file('admin.html') # 店員端 (新檔案)

# --- API 區 ---

# 客戶結帳
@app.route('/api/checkout', methods=['POST'])
def checkout():
    data = request.json
    c_name = data.get('customer_name')
    c_phone = data.get('customer_phone')
    cart_items = data.get('items')
    
    current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()

    receipt_lines = [] # 用來產生收據文字

    for item in cart_items:
        d_name = item['name']
        d_qty = int(item['qty'])
        d_price_unit = int(item['price'])
        option_str = f"({item.get('sugar')}/{item.get('ice')})"
        
        total_price = d_price_unit * d_qty
        
        # 寫入資料庫，status 預設為 'pending'
        c.execute("INSERT INTO orders (name, phone, item, qty, price, created_at, options, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                  (c_name, c_phone, d_name, d_qty, total_price, current_time, option_str, 'pending'))
        
        # 紀錄收據內容
        receipt_lines.append(f"{d_name} {option_str} x{d_qty} - ${total_price}")

    conn.commit()
    conn.close()
    
    # 回傳收據給前端顯示
    receipt_html = "<br>".join(receipt_lines)
    total_bill = sum(item['price'] * item['qty'] for item in cart_items)
    
    return jsonify({
        "status": "success", 
        "receipt_html": receipt_html,
        "total": total_bill,
        "time": current_time
    })

# 店員端：取得所有資料 (分為待製作 / 已完成)
@app.route('/api/admin/data', methods=['GET'])
def get_admin_data():
    with sqlite3.connect('shop.db') as conn:
        c = conn.cursor()
        c.execute("SELECT name, phone, item, qty, price, created_at, options, status FROM orders ORDER BY id DESC")
        rows = c.fetchall()
    
    pending_list = {}
    completed_list = {}

    for row in rows:
        # row: 0=name, 1=phone, 2=item, 3=qty, 4=price, 5=time, 6=options, 7=status
        key = (row[1], row[5]) # Key = 電話 + 時間
        status = row[7]
        
        target_dict = pending_list if status == 'pending' else completed_list
        
        opt_str = str(row[6]) if row[6] else ""
        item_display = f"{row[2]} {opt_str} x{row[3]}"

        if key not in target_dict:
            target_dict[key] = {
                "name": row[0],
                "phone": row[1],
                "items": [item_display],
                "total_qty": row[3],
                "total_price": row[4],
                "time": row[5]
            }
        else:
            target_dict[key]["items"].append(item_display)
            target_dict[key]["total_qty"] += row[3]
            target_dict[key]["total_price"] += row[4]

    # 轉成 List 回傳
    return jsonify({
        "pending": list(pending_list.values()),
        "completed": list(completed_list.values())
    })

# 店員端：出餐 (將狀態改為 completed)
@app.route('/api/complete', methods=['POST'])
def complete_order():
    data = request.json
    phone = data.get('phone')
    time = data.get('time')
    
    conn = sqlite3.connect('shop.db')
    c = conn.cursor()
    # 根據 電話 和 時間，把該單所有商品都改成 completed
    c.execute("UPDATE orders SET status = 'completed' WHERE phone = ? AND created_at = ?", (phone, time))
    conn.commit()
    conn.close()
    
    return jsonify({"status": "success"})

if __name__ == '__main__':
    app.run(port=5000, debug=True)