import React, {Component} from 'react'
import 'bootstrap/dist/css/bootstrap.min.css';
import {createItem, getAllCategories} from "../services/Service";

class Admin extends Component {

    constructor(props) {
        super(props);
        this.state = {
            categories: [],
            name: "",
            description: "",
            price: 0,
            category: "",
            image: "",
            submitted: false,
            error: false
        }
        this.handleChange = this.handleChange.bind(this);
    }


    createCategoriesOptions = () => {
        getAllCategories().then(response => this.setState({categories: response.categories.map((category) =>
                <option key={category.name}>{category.name}</option>)}));
    }

    componentDidMount() {
        this.createCategoriesOptions();
    }

    handleSubmit = (e) => {
        e.preventDefault();
        createItem(this.state).then(response => response === null ? this.setState({error: true}) : this.setState({submitted: true}));
    }

    handleChange = (e) => {
        const target = e.target;
        const value = target.value
        const id = target.id;

        this.setState({[id]: value});
    }

    Submitted = () => {
        return(
            <div className="alert alert-success show" role="alert">
                <strong>Item created!</strong>
            </div>
        )
    }

    Error = () => {
        return(
            <div className="alert alert-danger show" role="alert">
                <strong>Something went wrong</strong>
            </div>
        )
    }


    render() {
        return (
            <div className="col-6">
                <form>
                    <h2>Add item</h2>
                    <div className="form-group">
                        <label htmlFor="name">Name</label>
                        <input type="text" className="form-control" id="name" placeholder="Item name" onChange={this.handleChange}/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="description">Description</label>
                        <textarea className="form-control" id="description" placeholder="Item description" onChange={this.handleChange}/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="price">Price</label>
                        <input type="number" className="form-control" id="price" placeholder="Item price" onChange={this.handleChange}/>
                    </div>
                    <div className="form-group">
                        <label htmlFor="category">Category</label>
                        <select className="form-control" id="category" required onChange={this.handleChange}>
                            {this.state.categories}
                        </select>
                    </div>
                    <div className="form-group">
                        <label htmlFor="image">Image</label>
                        <input type="url" className="form-control" id="image" placeholder="Item image url" onChange={this.handleChange}/>
                    </div>
                    <button type="button" className="btn btn-primary mb-2" onClick={this.handleSubmit}>Create item</button>
                </form>
                {this.state.error ? this.Error() : (this.state.submitted ? this.Submitted() : null)}
            </div>
        );
    }
}

export default Admin;