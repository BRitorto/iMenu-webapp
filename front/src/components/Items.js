import React, {Component} from 'react'
import 'bootstrap/dist/css/bootstrap.min.css';
import {getAllCategories, getAllItemsByCategory} from "../services/Service";
import {capitalize} from "@material-ui/core";

class Items extends Component {

    state = {
        itemsByCategory: []
    }

    itemRow(item, index) {
        return(
                <tr key={index} className={index%2 === 0?'odd':'even'}>
                    <td>
                        <img width="60" height="60" src={item.image} alt="Item image"/>
                    </td>
                    <td>{item.name}</td>
                    <td>{item.description}</td>
                    <td>{item.price}</td>
                    <td><button type="button" className="btn btn-light" onClick={() => this.props.addItem(item)}>+</button></td>
                </tr>
        )
    }

    getCategories = () => {
        getAllCategories()
            .then(categories => {
                this.setState({categories: categories.categories})
                this.state.categories.forEach(category => {
                    let section = {}
                    section.category = category
                    getAllItemsByCategory(category.name)
                        .then(items => {
                            section.items = items.items.map((item, index) => this.itemRow(item, index))
                            this.setState({ itemsByCategory: [...this.state.itemsByCategory, section] });
                        });
                })
            })
    }

    componentDidMount() {
        this.getCategories();
    }

    render() {
        return (
            <div className="container">
                {
                    this.state.itemsByCategory.map(item => {return(
                        <div>
                            <h3>{capitalize(item.category.name)}</h3>
                                <table className="table">
                                    <thead>
                                        <tr>
                                            <th/>
                                            <th>Name</th>
                                            <th>Description</th>
                                            <th>Price</th>
                                            <th>Add to order</th>
                                        </tr>
                                    </thead>
                                    <tbody>{item.items}</tbody>
                                </table>
                        </div>
                    )})
                }
            </div>
        );
    }
}

export default Items;