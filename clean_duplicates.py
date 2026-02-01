import requests
from collections import defaultdict

# 設定 API 網址
GET_URL = "http://127.0.0.1:5000/api/products"
DELETE_URL_BASE = "http://127.0.0.1:5000/api/products/"

def clean_duplicates():
    print("🔍 正在掃描重複商品...")
    
    try:
        # 1. 取得所有商品
        response = requests.get(GET_URL)
        if response.status_code != 200:
            print(f"❌ 無法取得商品列表 (Status: {response.status_code})")
            return
        
        products = response.json()
        
        # 2. 按名稱分組
        name_groups = defaultdict(list)
        for p in products:
            name_groups[p['name']].append(p['id'])
        
        # 3. 檢查並刪除重複項
        total_deleted = 0
        for name, ids in name_groups.items():
            if len(ids) > 1:
                # 排序 ID，保留最大的 (最新的)
                ids.sort()
                to_keep = ids[-1]
                to_delete = ids[:-1]
                
                print(f"⚠️ 發現重複商品：【{name}】(共 {len(ids)} 筆)")
                
                for p_id in to_delete:
                    # 執行刪除
                    del_res = requests.delete(f"{DELETE_URL_BASE}{p_id}")
                    if del_res.status_code == 200:
                        print(f"   [刪除] ID: {p_id}，保留最新版 ID: {to_keep}")
                        total_deleted += 1
                    else:
                        print(f"   [失敗] 刪除 ID: {p_id} 時發生錯誤")
                        
        if total_deleted == 0:
            print("✨ 檢查完成，資料庫中沒有重複商品。")
        else:
            print(f"\n✅ 清理完畢！累計刪除 {total_deleted} 個重複品項。")
            
    except Exception as e:
        print(f"💥 腳本執行出錯：{str(e)}")

if __name__ == "__main__":
    clean_duplicates()
