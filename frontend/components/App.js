import '../css/base.css';
import React, { Component } from 'react';
import DbSelect from './DbSelect'
import TabTest from './TabTest'
import TabQuery from './TabQuery'
import TabInterL from './TabInterL'

function liTab(t, tab) {
  if (t.state.tab == tab) {
    return <li className="active"><a href="#">{tab}</a></li>;
  }
  else {
    return <li><a href="#" onClick={ function () { t.setState({tab: tab});} } >{tab}</a></li>;
  }
}

function getStatusLine(s) {
  if (s.statusLine == "") {
    return <div>Shoebox (c) 2017 - Frankfurt Haskell User Group</div>;
  } else 
  { 
    return <div><b>Status: </b>{s.statusLine}</div>;
  }
}

function contentSwitch(t) {
  if (t.state.tab == "Test") {
    return <TabTest cp={t.props.cp}/>
  }
  if (t.state.tab == "Query") {
    return <TabQuery cp={t.props.cp}/>
  }
  if (t.state.tab == "Inter-L") {
    return <TabInterL cp={t.props.cp}/>
  }
}

class App extends React.Component {

  setStatusLine(t) {
    this.setState({statusLine : t});
    setTimeout( () => {this.setState({statusLine : ""});}, 3000);
  }

  // render function 
  render() {
    return  (
  		<div className="container">
        {/* top row */} 
        <DbSelect cp={this.props.cp} statusF={this.setStatusLine}/>
        {/* middle row */} 
  			<div className="row middle-row">
  				<div className="col-sm-12">
  					<ul className="nav nav-tabs">
              {liTab(this, "Inter-L")}
              {liTab(this, "Query")}
              {liTab(this, "Test")}
  					</ul>
            {contentSwitch(this)}
  				</div>
  			</div>
        {/* bottom row */} 
  			<div className="row bottom-row">
          <div className="col-sm-12">
            { getStatusLine(this.state) }
          </div>
  			</div>
  		</div>
    );
  }

  // constructor
  constructor(props) {
    super(props);
    this.state = { statusLine : "", tab : "Test" };
    this.setStatusLine = this.setStatusLine.bind(this);
  }

}

export default App;
