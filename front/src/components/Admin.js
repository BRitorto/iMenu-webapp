import React, {Component} from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import {Link} from "react-router-dom";

class Admin extends Component {

    render() {
        return (
            <div className="App">
                <div className="container-fluid">
                    <div className="row mrgnbtm justify-content-md-center">
                        <div className="col-4">
                            <Link to="/admin/add_item"><button type="button" className="btn btn-primary mb-2" style={{margin: "1%"}}>Add items</button></Link>
                            <Link to="/admin/tables"><button type="button" className="btn btn-primary mb-2" style={{margin: "1%"}}>Manage tables</button></Link>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}

export default Admin;