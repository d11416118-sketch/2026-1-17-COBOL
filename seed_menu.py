import requests
import os

# 設定 API 網址 (確保伺服器正在執行)
API_URL = "http://127.0.0.1:5000/api/products"
STATIC_DIR = "static"

# 步驟 A: 清空舊資料 (Wipe)
def wipe_database():
    print("🗑️ 正在清空舊資料庫...")
    try:
        response = requests.get(API_URL)
        if response.status_code == 200:
            products = response.json()
            for p in products:
                del_url = f"{API_URL}/{p['id']}"
                requests.delete(del_url)
            print(f"✅ 已成功刪除 {len(products)} 個舊品項。")
        else:
            print("❌ 無法取得商品列表，清空失敗。")
    except Exception as e:
        print(f"💥 清空時發生錯誤: {str(e)}")

# 步驟 B: 定義精準素材 (Define Assets)
IMAGES = {
    "icecream": "static/img_icecream.jpg",
    "yakult": "static/img_yakult.jpg",
    "honeylemon": "static/img_honeylemon.jpg", # 淡黃色、兩片檸檬在杯口
    "tea_yellow": "static/img_tea_yellow.jpg",
    "tea_red": "static/img_tea_red.jpg",
    "milktea_pearl": "static/img_milktea_pearl.jpg",
    "milktea": "static/img_milktea.jpg",       # 純奶茶 (專供黑糖奶茶)
    "freshmilk": "static/img_freshmilk.jpg",   # 熊貓珍珠鮮奶 (整杯、底部有珍珠)
    "passion": "static/img_passion.jpg",
    "dark": "static/img_dark.jpg",             # 鐵觀音 (較淡版本)
    "taro": "static/img_taro.jpg",             # 芋見泥
    "grassjelly": "static/img_grassjelly.jpg",
    "pudding": "static/img_pudding.jpg",
    "coconutjelly": "static/img_coconutjelly.jpg",
    "qq": "static/img_qq.jpg",                 # 珍珠+椰果
    "lemon": "static/img_lemon.jpg",           # 港式凍檸
    "wintermelon": "static/img_wintermelon.jpg",           # 冬瓜玉露 (深褐色)
    "wintermelon_lemon": "static/img_wintermelon_lemon.jpg", # 檸檬冬瓜
    "latte": "static/img_latte.jpg",                       # 花鳥那堤 (漸層)
    "freshmilktea_common": "static/img_freshmilktea_common.jpg", # 通用鮮奶茶 (漸層)
    "oatmilktea": "static/img_oatmilktea.jpg"               # 燕麥仁奶茶
}

# 升級後的智慧分配圖片函式
def get_image_path(name):
    n = name.lower()
    # 優先權最高：精確匹配特定品項
    if "冬瓜玉露" in n: return IMAGES["wintermelon"]
    if "檸檬冬瓜" in n: return IMAGES["wintermelon_lemon"]
    if "花鳥那堤" in n: return IMAGES["latte"]
    if "熊貓珍珠" in n: return IMAGES["freshmilk"]
    if "芋見泥" in n: return IMAGES["taro"]
    if "黑糖奶茶" in n or "醇香奶茶" in n or "泰式奶茶" in n: return IMAGES["milktea"]
    if "燕麥" in n: return IMAGES["oatmilktea"]
    
    # 次優先：特殊配料
    if "冰淇淋" in n: return IMAGES["icecream"]
    if "多多" in n: return IMAGES["yakult"]
    if "仙草" in n: return IMAGES["grassjelly"]
    if "布丁" in n: return IMAGES["pudding"]
    if "椰果" in n: return IMAGES["coconutjelly"]
    if "港式凍檸" in n: return IMAGES["lemon"]
    
    # 系列關鍵字
    if "鮮奶" in n or "拿掛" in n or "拿鐵" in n or "那堤" in n: return IMAGES["freshmilktea_common"]
    if "珍珠" in n or "波霸" in n or "奶茶" in n or "奶" in n: return IMAGES["milktea_pearl"]
    if "冬瓜" in n or "黑糖" in n or "觀音" in n or "鐵觀音" in n: return IMAGES["dark"]
    if "檸檬" in n or "鮮檸" in n or "蜂蜜" in n: return IMAGES["honeylemon"]
    if "百香" in n: return IMAGES["passion"]
    if "紅" in n: return IMAGES["tea_red"]
    return IMAGES["tea_yellow"]

# 步驟 C: 重建菜單 (Re-seed)
menu_items = [
    # 基礎品項
    {"name": "錫蘭紅茶", "price": 35},
    {"name": "文山包種茶", "price": 35},
    {"name": "炭焙烏龍", "price": 35},
    {"name": "伯爵奶茶", "price": 55},
    {"name": "紅茶拿鐵", "price": 65},
    
    # 原茶類
    {"name": "茉香綠茶", "price": 35},
    {"name": "四季春青茶", "price": 35},
    {"name": "金萱茶", "price": 35},
    {"name": "鐵觀音", "price": 35},
    {"name": "格雷伯爵紅茶", "price": 35},
    {"name": "冬瓜玉露", "price": 35},
    {"name": "桂花烏龍", "price": 50},
    {"name": "花鳥那堤", "price": 70},
    
    # 鮮奶茶類
    {"name": "紅/綠鮮奶茶", "price": 65},
    {"name": "格雷伯爵鮮奶", "price": 65},
    {"name": "金萱/觀音鮮奶", "price": 65},
    {"name": "熊貓珍珠鮮奶", "price": 60},
    {"name": "可可/阿華田鮮奶", "price": 65},
    {"name": "黑糖/冬瓜鮮奶", "price": 65},
    {"name": "布丁鮮奶", "price": 60},
    {"name": "仙草凍鮮奶", "price": 60},
    {"name": "芋見泥", "price": 65},

    # 奶茶特調
    {"name": "醇香奶茶", "price": 55},
    {"name": "珍珠奶茶", "price": 60},
    {"name": "泰式奶茶", "price": 65},
    {"name": "布丁奶茶", "price": 65},
    {"name": "仙草凍奶茶", "price": 55},
    {"name": "椰果奶茶", "price": 60},
    {"name": "黑糖奶茶", "price": 60},
    {"name": "燕麥仁奶茶", "price": 60},

    # 鮮果/手作特調
    {"name": "鮮檸蜜蘆薈", "price": 65},
    {"name": "翡翠檸檬青", "price": 60},
    {"name": "蜂蜜檸檬", "price": 60},
    {"name": "檸檬冬瓜", "price": 60},
    {"name": "百香果綠", "price": 60},
    {"name": "百香多多", "price": 70},
    {"name": "港式凍檸茶", "price": 60},
    {"name": "冰淇淋紅茶", "price": 55},
    {"name": "多多綠茶", "price": 55}
]

def reseed_database():
    print(f"🚀 開始重灌 {len(menu_items)} 個商品...")
    success_count = 0
    used_images = set()
    for i, item in enumerate(menu_items, 1):
        img_path = get_image_path(item["name"])
        if not os.path.exists(img_path):
            print(f"[{i}/{len(menu_items)}] ⚠️ 圖片缺失跳過：{item['name']} (找不到 {img_path})")
            continue
        try:
            with open(img_path, 'rb') as f:
                files = {'image': f}
                data = {'name': item["name"], 'price': item["price"]}
                response = requests.post(API_URL, data=data, files=files)
                if response.status_code in [200, 201]:
                    print(f"[{i}/{len(menu_items)}] ✅ 成功：{item['name']} ${item['price']}")
                    success_count += 1
                    used_images.add(os.path.basename(img_path))
                else:
                    print(f"[{i}/{len(menu_items)}] ❌ 失敗：{item['name']} (Status: {response.status_code})")
        except Exception as e:
            print(f"[{i}/{len(menu_items)}] 💥 錯誤：{item['name']} - {str(e)}")
    print(f"\n✨ 上架完成！共成功上架 {success_count} 個項目。")
    return used_images

# 步驟 D: 自動清理 (Cleanup)
def cleanup_unused_images(used_images_filenames):
    print("\n🧹 正在清理 static/ 資料夾中未使用的圖片...")
    # 這些是系統必要的檔案，不可刪除
    preserved_files = {
        "banner.png", "black_tea.png", "green_tea.png", "latte.png", 
        "milk_tea.png", "oolong.png", "測試上架下架.png"
    }
    
    deleted_count = 0
    try:
        for filename in os.listdir(STATIC_DIR):
            file_path = os.path.join(STATIC_DIR, filename)
            # 只處理圖片檔案
            if os.path.isfile(file_path) and filename.lower().endswith(('.jpg', '.jpeg', '.png')):
                if filename not in used_images_filenames and filename not in preserved_files:
                    try:
                        os.remove(file_path)
                        print(f"   [刪除] 未使用圖片: {filename}")
                        deleted_count += 1
                    except Exception as e:
                        print(f"   [失敗] 刪除 {filename} 時出錯: {e}")
        print(f"✅ 清理完畢！共刪除 {deleted_count} 個冗餘檔案。")
    except Exception as e:
        print(f"💥 清理時發生錯誤: {str(e)}")

if __name__ == "__main__":
    wipe_database()
    used_img_names = reseed_database()
    cleanup_unused_images(used_img_names)
