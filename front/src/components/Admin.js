import React, {Component} from 'react'
import 'bootstrap/dist/css/bootstrap.min.css';
import {getAllCategories} from "../services/ItemService";

class Admin extends Component {

    state = {
        categories: []
    }

    createCategoriesOptions = () => {
        getAllCategories().then(response => this.setState({categories: response.categories.map((category) =>
                <option key={category.name}>{category.name}</option>)}));
    }

    componentDidMount() {
        this.createCategoriesOptions();
    }

    render() {
        return (
            <div className="col-6">
                <form>
                    <h2>Add item</h2>
                    <div className="form-group">
                        <label htmlFor="name">Name</label>
                        <input type="text" className="form-control" id="name" placeholder="Item name" required/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="description">Description</label>
                        <textarea className="form-control" id="description" placeholder="Item description" required/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="price">Price</label>
                        <input type="number" className="form-control" id="price" placeholder="Item price" required/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="category">Category</label>
                        <select className="form-control" id="category" required>
                            {this.state.categories}
                        </select>
                    </div>
                    <div className="form-group">
                        <label htmlFor="image">Image</label>
                        <input type="file" className="form-control-file" id="image"/>
                    </div>
                </form>
            </div>
        );
    }
}

export default Admin;