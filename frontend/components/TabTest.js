import React, { Component } from 'react';

class TabTest extends React.Component {
  // render function 
  render() {
  	return (
		<div>
			<h3><small>Test Panel</small></h3>
			you can try the commands: "current-db", "save-db", "available-dbs", "open-db [db]", "save-db-as [db]"
			<p/>

			<div className="form-group">
				<label for="command">Command:</label>
				<input type="text" className="form-control" id="command"></input>
			</div>
			<p/>

			<button className="btn btn-primary" onClick={ () => {
				var cmds = $('#command')[0].value.split(/\s+/);
				this.props.sbc.callShoebox(cmds[0], cmds.length > 1 ? cmds[1] : null, (d) => this.setState({testResult : d.toString()}));
			}}>
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