const username = 'username'
const password = 'password'

const token = Buffer.from(`${username}:${password}`, 'utf8').toString('base64')

export async function getAllItems() {
    const response = await fetch('/api/items');
    return response.json();
}

export async function createOrder(data, table) {
    const slugs = data.map(item => (item.slug))
    const response = await fetch(`/api/orders`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({tableNumber: table, items: slugs})
    });
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function finishOrder(table) {
    const response = await fetch(`/admin/orders/` + table, {
        method: 'DELETE',
        headers: {'Content-Type': 'application/json', 'Authorization': `Basic ${token}`},
    });
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function getAllOrders() {
    const response = await fetch('/admin/orders', {
        method: 'GET',
        headers: {'Content-Type': 'application/json', 'Authorization': `Basic ${token}`},
    });
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function getAllCategories() {
    const response = await fetch('/api/categories');
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function getAllItemsByCategory(category) {
    const response = await fetch(`/api/items/` + category);
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function createItem(data) {
    const response = await fetch(`/admin/items`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json', 'Authorization': `Basic ${token}`},
        body: JSON.stringify({
            name: data.name,
            description: data.description,
            price: data.price,
            category: data.category,
            image: data.image
        })
    });
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}