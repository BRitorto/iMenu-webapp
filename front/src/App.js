import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import {Component} from "react";
import Items from './components/Items'
import Order from "./components/Order"
import Admin from "./components/Admin"
import AddItem from "./components/AddItem"
import Tables from "./components/Tables"
import {createOrder} from "./services/Service";
import Table from "./components/Table"
import {Ops} from "./components/Ops"
import {
    BrowserRouter as Router,
    Switch,
    Route
} from "react-router-dom";
import {OrderSent} from "./components/OrderSent";

class App extends Component {

  state = {
    items: [],
    numberOfItems: 0,
    orderItems: [],
    ordered: false,
    table: 1,
    error: false,
    categories: []
  }

    addItem = (item) => {
        this.setState({ orderItems: [...this.state.orderItems, item] });
    }

    removeItem = (item) => {
        const array = [...this.state.orderItems];
        const index = array.indexOf(item)
        if (index !== -1) {
            array.splice(index, 1);
            this.setState({orderItems: array});
        }
    }

    sendOrder = () => {
      createOrder(this.state.orderItems, this.state.table).then(response => response == null ? this.setState({error: true}) : this.setState({ordered: true}))
    }

    chooseTable = (e) => {
      this.setState({table: parseInt(e.target.value)})
      console.log(this.state.table)
    }

    Menu = () => {
          const mainMenu = (<div className="App">
            <div className="container-fluid">
                <div className="row mrgnbtm">
                    <div className="container-fluid">
                        <div className="col-2">
                            <Table chooseTable={this.chooseTable.bind(this)}/>
                        </div>
                    </div>
                </div>
                <div className="row mrgnbtm">
                    <div className="col-8">
                        <Items addItem={this.addItem.bind(this)}/>
                    </div>
                    <div className="col-4">
                        <Order items={this.state.orderItems}
                               key={this.state.orderItems.length}
                               removeItem={this.removeItem.bind(this)}
                               sendOrder={this.sendOrder.bind(this)}
                        />
                    </div>
                </div>
            </div>
        </div>);

        const ordered = (<div className="App"><OrderSent/></div>);

        const error = (<div className="App"><Ops/></div>)

        if (this.state.error) {
            return error;
        }
        return (this.state.ordered ? ordered : mainMenu);
    }

    render() {
        return(
            <Router>
                    <div className="container-fluid">
                        <nav className="navbar navbar-expand-lg navbar-dark bg-dark fixed-top">
                            <ul className="navbar-nav mr-auto">
                                <li className="nav-item">
                                    <a className="nav-link" href="/">iMenu</a>
                                </li>
                                <li className="nav-item">
                                    <a className="nav-link" href="/admin">Administrator</a>
                                </li>
                            </ul>
                    </nav>
                    </div>
                    <Switch>
                        <Route exact path="/admin" component={Admin}/>
                        <Route path="/admin/add_item" component={AddItem}/>
                        <Route path="/admin/tables" component={Tables}/>
                        <Route exact path="/">
                            {this.Menu()}
                        </Route>
                    </Switch>
            </Router>
        );
    }
}

export default App;
