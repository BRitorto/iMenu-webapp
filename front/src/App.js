import logo from './logo.svg';
import './App.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import {Component} from "react";
import {getAllItems} from "./services/ItemService";
import {Items} from './components/Items'
import {DisplayBoard} from "./components/DisplayBoard";
import {Header} from "./components/Header";

class App extends Component {

  state = {
    items: [],
    numberOfItems: 0
  }

  getAllItems = () => {
      getAllItems()
        .then(items => {
            console.log(items)
            this.setState({items: items, numberOfItems: items.length})
        });
    }

    render() {
        return (
            <div className="App">
                <Header></Header>
                <div className="container mrgnbtm">
                    <div className="row">
                        <div className="col-md-4">
                            <DisplayBoard
                                numberOfItems={this.state.numberOfItems}
                                getAllItems={this.getAllItems}
                            >
                            </DisplayBoard>
                        </div>
                    </div>
                </div>
                <div className="row mrgnbtm">
                    <Items items={this.state.items}></Items>
                </div>
            </div>
        );
    }
}


/*function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  );
}


                        <div className="col-md-8">
                            <CreateUser
                                user={this.state.user}
                                onChangeForm={this.onChangeForm}
                                createUser={this.createUser}
                            >
                            </CreateUser>
                        </div>
*/

export default App;
