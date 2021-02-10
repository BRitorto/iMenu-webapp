import React, {Component} from 'react'
import 'bootstrap/dist/css/bootstrap.min.css';

class Table extends Component {

    render() {
        return (<form>
            <h2>Table number</h2>
            <select className="form-control" id="table"
                    onChange={this.props.chooseTable}>
                <option value={1}>1</option>
                <option value={2}>2</option>
                <option value={3}>3</option>
                <option value={4}>4</option>
                <option value={5}>5</option>
            </select>
        </form>);
    }
}

export default Table;