import React, { Component } from 'react';

function showLog(log) {
  return log.map( (d) => 
    <div>{JSON.stringify(d)}<br/></div>
    )
}

class TabTest extends React.Component {
  // render function 
  render() {
  	return (
		<div>
			<h3><small>Test Panel</small></h3>

			<h5>Execute Cmd</h5>
			<div className="form-group">
				<label for="command">Command:</label>
				<input type="text" className="form-control" id="command"></input>
			</div>
			<button className="btn btn-primary" onClick={ () => {
				var cmds = $('#command')[0].value.split(/\s+/);
				this.props.sbc.callShoebox(cmds[0], cmds.length > 1 ? cmds[1] : null, (d) => this.setState({testResult : d.toString()}));
			}}>
			Execute
			</button>
			<h5>Event Log</h5>
			<div style={{maxHeight: 200 + "px", overflowY: "scroll"}}>
			  { showLog(this.state.msgLog) }
			</div> 
		</div>
  		);
  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { msgLog : [] };
    this.update = this.update.bind(this);
  	}

    update() {
      this.setState({msgLog : this.props.sbc.getLog()}); 
    }

    componentDidMount() {
      this.update(); 
      this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
      this.props.sbc.unsubscribe(this.update);
    }
}

export default TabTest; 