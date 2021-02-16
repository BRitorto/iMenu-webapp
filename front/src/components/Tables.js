import React, {Component} from "react";
import {finishOrder, getAllOrders} from "../services/Service";

class Tables extends Component {

    state = {
        orders: [],
        orderCards: {}
    }

    componentDidMount() {
        getAllOrders().then(orders => {
            this.setState({orders: orders.orders})});
    }

    handleSubmit = (e) => {
        e.preventDefault();
        finishOrder(e.target.id).then(() => window.location.reload());
    }

    render() {
        return(
            <div className="App">
                <div className="container-fluid">
                    <div className="row mrgnbtm justify-content-md-center">
                        {
                            this.state.orders.map(order => { return(
                                <div className="card" style={{margin: "1%"}}>
                                    <div className="card-body">
                                        <h5 className="card-title"> Table {order.table}</h5>
                                        <p className="card-text">Items:
                                            <br/>
                                            {
                                                order.itemsSlug.map(slug => {return(
                                                    <div>
                                                        {"- " + slug.substring(slug.indexOf('-') + 1, slug.length).replaceAll('-', ' ')}
                                                    </div>
                                                )})
                                            }
                                        </p>
                                        <button type="button" className="btn btn-primary mb-2" id={order.table} onClick={this.handleSubmit}>Finish order</button>
                                    </div>
                                </div>
                            )})
                        }
                    </div>
                </div>
            </div>
            );
        }
}

export default Tables;