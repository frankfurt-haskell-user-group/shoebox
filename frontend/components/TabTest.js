import React, { Component } from 'react';

class TabTest extends React.Component {
  // render function 
  render() {
  	return (
		<div>
			<h3><small>Test Panel</small></h3>
			you can try the commands: "current-db", "save-db", "available-dbs"
			<p/>

			<div className="form-group">
				<label for="command">Command:</label>
				<input type="text" onChange={ () => console.log($('#command')[0].value) } className="form-control" id="command"></input>
			</div>
			<p/>

			<button className="btn btn-primary" onClick={ () => console.log("test clicked") }>
			Execute
			</button>
			<p/>
			Test Result: 
			<p/>
			{this.state.testResult}
		</div>
  		);
  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { testResult : ""};
  	}

}

export default TabTest; 