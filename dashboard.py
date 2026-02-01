import sqlite3
import pandas as pd
import streamlit as st

# --- 設定頁面資訊 ---
st.set_page_config(
    page_title="1959 製茶所 - 資料庫全景透視",
    page_icon="👁️",
    layout="wide"
)

# --- 1. 連線並讀取所有資料表 ---
def get_all_data():
    # 加入 timeout 防止鎖死
    conn = sqlite3.connect('shop.db', timeout=10)
    
    # 1. 會員資料 (含帳密)
    df_users = pd.read_sql("SELECT * FROM users", conn)
    
    # 2. 員工打卡 (Attendance)
    df_attendance = pd.read_sql("SELECT * FROM attendance ORDER BY id DESC", conn)
    
    # 3. 訂單資訊 (Orders)
    df_orders = pd.read_sql("SELECT * FROM orders ORDER BY id DESC", conn)
    
    # 4. 產品列表 (Products)
    df_products = pd.read_sql("SELECT * FROM products", conn)
    
    conn.close()
    return df_users, df_attendance, df_orders, df_products

# --- 2. 載入資料 ---
try:
    df_users, df_attendance, df_orders, df_products = get_all_data()
except Exception as e:
    st.error(f"❌ 讀取失敗，請確認 shop.db 是否存在。\n錯誤訊息: {e}")
    st.stop()

# --- 3. 標題區 ---
st.title("👁️ 1959 製茶所 | 資料庫上帝視角")
st.caption("即時監控 SQLite 資料庫內的所有 Raw Data")
st.markdown("---")

# --- 4. 分頁顯示各類資料 ---
# 建立四個分頁
tab1, tab2, tab3, tab4 = st.tabs([
    "👥 會員帳密管理", 
    "🕰️ 員工打卡紀錄", 
    "📦 所有訂單明細", 
    "🍹 飲料品項清單"
])

# === 分頁 1: 會員資料 ===
with tab1:
    st.subheader(f"會員資料表 (共 {len(df_users)} 人)")
    st.warning("⚠️ 注意：密碼欄位已透過 Hash 加密，這是正常的安全機制，無法直接看到明文。")
    if not df_users.empty:
        # 顯示資料表，並設定寬度自動延展
        st.dataframe(
            df_users, 
            column_config={
                "password": st.column_config.TextColumn("加密密碼 (Hashed)", help="為了安全，密碼在資料庫中是亂碼"),
                "created_at": "註冊時間",
                "phone": "手機號碼"
            },
            use_container_width=True
        )
    else:
        st.info("目前沒有會員資料")

# === 分頁 2: 打卡紀錄 ===
with tab2:
    st.subheader(f"員工打卡流水帳 (共 {len(df_attendance)} 筆)")
    if not df_attendance.empty:
        st.dataframe(
            df_attendance,
            column_config={
                "staff_name": "員工姓名",
                "action_type": st.column_config.TextColumn("動作", help="上班/下班"),
                "log_time": "打卡時間"
            },
            use_container_width=True
        )
    else:
        st.info("目前沒有打卡紀錄")

# === 分頁 3: 訂單明細 ===
with tab3:
    st.subheader(f"歷史訂單總覽 (共 {len(df_orders)} 筆)")
    
    # 增加一個篩選器
    filter_status = st.radio("篩選訂單狀態:", ["全部", "pending (待製作)", "completed (已完成)"], horizontal=True)
    
    # 根據篩選器過濾資料
    display_df = df_orders
    if filter_status == "pending (待製作)":
        display_df = df_orders[df_orders['status'] == 'pending']
    elif filter_status == "completed (已完成)":
        display_df = df_orders[df_orders['status'] == 'completed']
        
    st.dataframe(display_df, use_container_width=True)

# === 分頁 4: 產品列表 ===
with tab4:
    st.subheader(f"目前上架飲品 (共 {len(df_products)} 品項)")
    if not df_products.empty:
        st.dataframe(
            df_products,
            column_config={
                "image": st.column_config.ImageColumn("圖片預覽", help="對應 static 資料夾的圖片"),
                "price": st.column_config.NumberColumn("價格", format="$%d")
            },
            use_container_width=True
        )
    else:
        st.info("目前沒有上架商品")

# --- 側邊欄功能 ---
with st.sidebar:
    st.header("⚙️ 控制台")
    if st.button("🔄 重新整理資料庫"):
        st.rerun()
    st.markdown("---")
    st.write(f"資料庫連線狀態: 🟢 線上")
    st.write(f"最後更新: {pd.Timestamp.now().strftime('%H:%M:%S')}")