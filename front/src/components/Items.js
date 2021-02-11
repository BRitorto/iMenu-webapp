import React, {Component} from 'react'
import 'bootstrap/dist/css/bootstrap.min.css';
import {getAllItems} from "../services/Service";

class Items extends Component {

    state = {itemsTable: ""}

    itemRow(item, index) {
        return(
                <tr key={index} className={index%2 === 0?'odd':'even'}>
                    <td>
                        <img width="60" height="60" src={item.image} alt="Item image"/>
                    </td>
                    <td>{item.name}</td>
                    <td>{item.description}</td>
                    <td>{item.price}</td>
                    <td>{item.category}</td>
                    <td><button type="button" className="btn btn-light" onClick={() => this.props.addItem(item)}>+</button></td>
                </tr>
        )
    }

    componentDidMount() {
        getAllItems()
            .then(items => {
                this.setState({itemsTable: items.items.map((item, index) => this.itemRow(item, index))})
            });
    }

    render() {
        return (
            <div className="container">
                <h2>Items</h2>
                <table className="table">
                    <thead>
                    <tr>
                        <th/>
                        <th>Name</th>
                        <th>Description</th>
                        <th>Price</th>
                        <th>Category</th>
                        <th>Add to order</th>
                    </tr>
                    </thead>
                    <tbody>{this.state.itemsTable}</tbody>
                </table>
            </div>
        );
    }
}

export default Items;