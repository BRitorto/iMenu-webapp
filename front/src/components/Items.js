import React from 'react'

export const Items = ({items}) => {

    const itemsArray = items.items === undefined ? items : items.items
    console.log('items length:::', itemsArray.length)
    if (itemsArray.length === 0) return null

    const ItemRow = (item,index) => {
        return(
            <tr key = {index} className={index%2 === 0?'odd':'even'}>
                <td>{index + 1}</td>
                <td>{item.name}</td>
                <td>{item.description}</td>
                <td>{item.price}</td>
                <td>{item.category}</td>
            </tr>
        )
    }

    const itemTable = itemsArray.map((item,index) => ItemRow(item,index))

    return(
        <div className="container">
            <h2>Items</h2>
            <table className="table table-bordered">
                <thead>
                <tr>
                    <th>Item number</th>
                    <th>Name</th>
                    <th>Description</th>
                    <th>Price</th>
                    <th>Category</th>
                </tr>
                </thead>
                <tbody>
                {itemTable}
                </tbody>
            </table>
        </div>
    )
}