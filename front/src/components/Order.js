import React, {Component} from 'react'

class Order extends Component {

    state = {itemTable : ""}

    itemTable() {
        if (this.props.items.length !== 0) {
            return (this.props.items.map((item, index) => this.itemRow(item, index)));
        }
        return null
    }

    itemRow (item, index) {
        return(
            <tr key = {index} className={index%2 === 0?'odd':'even'}>
                <td>{item.name}</td>
                <td>{item.price}</td>
                <td><button type="button" className="btn btn-light" onClick={() => this.props.removeItem(item)}> - </button></td>
            </tr>
        )
    }


    componentDidMount() {
        this.setState({itemTable: this.itemTable()});
    }

    render() {
        return (
            <div className="container">
                <h2>Order</h2>
                <table className="table">
                    <thead>
                    <tr>
                        <th>Name</th>
                        <th>Price</th>
                        <th/>
                    </tr>
                    </thead>
                    <tbody>{this.state.itemTable}</tbody>
                </table>
                {this.props.items.length > 0 &&
                    <button type="button" className="btn btn-primary" onClick={() => this.props.sendOrder()}> Send order </button>
                }
            </div>
        );
    }
}

export default Order;