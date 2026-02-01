/* static/js/admin.js */

// =======================
// 1. 初始化與頁面切換邏輯
// =======================
document.addEventListener('DOMContentLoaded', () => {
    // 啟動自動更新 (每 3 秒一次)
    setInterval(fetchAdminData, 3000);
    fetchAdminData(); // 立即執行一次
    
    // 載入商品列表 (只需一次)
    fetchProducts();
});

function switchTab(tabId) {
    // 隱藏所有區塊
    document.querySelectorAll('main > section').forEach(el => el.classList.add('hidden'));
    document.querySelectorAll('.sidebar-link').forEach(el => el.classList.remove('active'));

    // 顯示目標區塊
    document.getElementById('section-' + tabId).classList.remove('hidden');
    document.getElementById('tab-btn-' + tabId).classList.add('active');
}

// =======================
// 2. 核心：後台資料自動同步
// =======================
async function fetchAdminData() {
    try {
        const res = await fetch('/api/admin/data');
        if (!res.ok) throw new Error("API Error");
        const data = await res.json();

        renderPending(data.pending);
        renderCompleted(data.completed);
        renderAttendance(data.attendance);
    } catch (e) {
        console.error("同步失敗:", e);
    }
}

// 渲染「待製作訂單」 (黃色卡片)
function renderPending(orders) {
    const container = document.getElementById('pending-list');
    
    // 如果沒單，顯示空狀態
    if (!orders || orders.length === 0) {
        container.innerHTML = '<div class="col-span-full text-center text-gray-400 py-12 bg-gray-50 rounded-2xl border border-dashed border-gray-200">🍵 目前沒有待製作訂單，休息一下吧！</div>';
        return;
    }

    // 生成卡片 HTML
    container.innerHTML = orders.map(o => `
        <div class="bg-white p-6 rounded-2xl shadow-sm border-l-4 border-amber-400 flex flex-col justify-between hover:shadow-md transition">
            <div class="mb-4">
                <div class="flex justify-between items-start mb-2">
                    <span class="font-bold text-xl text-gray-800">${o.item}</span>
                    <span class="bg-rose-100 text-rose-600 px-2 py-1 rounded text-xs font-bold">$${o.price}</span>
                </div>
                <div class="flex items-center text-sm text-gray-500 mb-1">
                    <span class="bg-gray-100 px-2 py-0.5 rounded mr-2 text-xs">x${o.qty}</span>
                    <span class="font-mono text-xs text-gray-400">${o.options}</span>
                </div>
                <div class="flex items-center text-gray-400 text-xs mt-2">
                    <i data-lucide="user" class="w-3 h-3 mr-1"></i> ${o.name} (${o.phone.slice(-3)})
                </div>
            </div>
            <button onclick="markComplete('${o.phone}', '${o.time}')" 
                class="w-full bg-gray-800 text-white py-3 rounded-xl font-bold hover:bg-black transition text-sm flex items-center justify-center group">
                <span>完成訂單</span>
                <i data-lucide="check" class="w-4 h-4 ml-2 group-hover:scale-110 transition-transform"></i>
            </button>
        </div>
    `).join('');
    
    // 重新渲染圖標
    if(window.lucide) lucide.createIcons();
}

// 渲染「近期完成」
function renderCompleted(orders) {
    const container = document.getElementById('completed-list');
    container.innerHTML = orders.map(o => `
        <div class="bg-white p-4 rounded-xl border border-gray-100 opacity-60 hover:opacity-100 transition flex justify-between items-center">
             <div>
                <div class="font-bold text-gray-700">${o.item} <span class="text-xs font-normal">x${o.qty}</span></div>
                <div class="text-[10px] text-gray-400 font-mono">${o.time.split(' ')[1]}</div>
             </div>
             <div class="text-xs text-gray-400">${o.name}</div>
        </div>
    `).join('');
}

// 渲染「打卡紀錄」
function renderAttendance(logs) {
    const container = document.getElementById('attendance-list');
    container.innerHTML = logs.map(l => `
        <tr class="border-b border-gray-50 last:border-0 hover:bg-gray-50 transition">
            <td class="py-3 px-2 text-xs text-gray-400 font-mono">${l.time.split(' ')[1]}</td>
            <td class="py-3 px-2 font-bold text-gray-700">${l.name}</td>
            <td class="py-3 px-2">
                <span class="${l.action === '上班' ? 'bg-emerald-100 text-emerald-600' : 'bg-gray-100 text-gray-600'} px-2 py-1 rounded text-xs font-bold">
                    ${l.action}
                </span>
            </td>
        </tr>
    `).join('');
}

// =======================
// 3. 動作邏輯 (完成訂單、打卡)
// =======================

// 完成訂單
async function markComplete(phone, time) {
    if(!confirm('確定已製作完成？')) return;
    
    try {
        await fetch('/api/complete', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ phone, time })
        });
        // 成功後會自動透過 setInterval 更新，這裡也可以手動立即更新
        fetchAdminData();
        showToast('訂單已完成！');
    } catch (e) {
        alert('操作失敗');
    }
}

// 員工打卡
async function clockIn(action) {
    const name = document.getElementById('staff-name').value;
    if(!name) return alert('請先輸入員工姓名');

    try {
        await fetch('/api/clockin', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ name, action })
        });
        document.getElementById('staff-name').value = ''; // 清空輸入框
        fetchAdminData(); // 立即更新列表
        showToast(`${name} ${action}打卡成功！`);
    } catch (e) {
        alert('打卡失敗');
    }
}

// =======================
// 4. 商品管理邏輯 (搜尋、刪除、預覽)
// =======================
let allProducts = [];

async function fetchProducts() {
    const res = await fetch('/api/products');
    allProducts = await res.json();
    renderProductList(allProducts);
}

function renderProductList(products) {
    const container = document.getElementById('product-list');
    container.innerHTML = products.map(p => `
        <div class="flex items-center justify-between p-4 hover:bg-gray-50 transition group">
            <div class="flex items-center space-x-4">
                <div class="w-12 h-12 rounded-xl bg-gray-100 overflow-hidden border border-gray-200">
                    <img src="/static/${p.image}" class="w-full h-full object-cover" onerror="this.src='/static/tea_yellow.jpg'">
                </div>
                <div>
                    <div class="font-bold text-gray-800">${p.name}</div>
                    <div class="text-xs text-rose-500 font-bold">$${p.price}</div>
                </div>
            </div>
            <button onclick="deleteProduct(${p.id})" class="text-gray-300 hover:text-red-500 p-2 opacity-0 group-hover:opacity-100 transition">
                <i data-lucide="trash-2" class="w-4 h-4"></i>
            </button>
        </div>
    `).join('');
    if(window.lucide) lucide.createIcons();
}

// 搜尋過濾
function filterProducts(keyword) {
    const filtered = allProducts.filter(p => p.name.includes(keyword));
    renderProductList(filtered);
}

// 刪除商品
async function deleteProduct(id) {
    if(!confirm('確定要下架此商品嗎？')) return;
    await fetch(`/api/products/${id}`, { method: 'DELETE' });
    fetchProducts();
}

// 新增商品
async function handleAddProduct(e) {
    e.preventDefault();
    const form = e.target;
    const formData = new FormData(form);
    
    await fetch('/api/products', {
        method: 'POST',
        body: formData
    });
    
    form.reset();
    resetPreview();
    fetchProducts();
    showToast('商品上架成功！');
}

// 圖片預覽
function previewImage(input) {
    if (input.files && input.files[0]) {
        const reader = new FileReader();
        reader.onload = function(e) {
            document.getElementById('image-preview').src = e.target.result;
            document.getElementById('preview-container').classList.remove('hidden');
        }
        reader.readAsDataURL(input.files[0]);
    }
}

function resetPreview() {
    document.getElementById('preview-container').classList.add('hidden');
    document.getElementById('image-preview').src = '';
}

// 通用 Toast 通知 (如果 app.js 沒有定義，這裡補一個簡單的)
function showToast(msg) {
    const div = document.createElement('div');
    div.className = 'fixed bottom-4 right-4 bg-gray-800 text-white px-6 py-3 rounded-xl shadow-xl z-50 animate-bounce';
    div.innerText = msg;
    document.body.appendChild(div);
    setTimeout(() => div.remove(), 3000);
}