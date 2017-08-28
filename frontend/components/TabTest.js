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
		<div className="row">
			<div className="col-sm-12">
				<h3><small>Execute Cmd</small></h3>
				commands: available-dbs, current-db, open-db db, save-db, save-db-as db, query text, ... <p/>
			</div>
		</div>
		<div className="row">
			<div className="form-group col-sm-8">
				<input style={{width: 500 + "px"}} type="text" className="form-control" id="command"></input>
			</div>
			<div className="col-sm-4">
				<button className="btn btn-primary btn-sm" onClick={ () => {
					var cmds = $('#command')[0].value.split(/\s+/);
					this.props.sbc.callShoebox(cmds[0], cmds.length > 1 ? cmds[1] : null);
				}}>
				Execute
				</button>
			</div>
		</div>
		<div className="row">
			<div className="col-sm-12">
				<h3><small>Event Log</small></h3>
				<div style={{maxHeight: 250 + "px", overflowY: "scroll"}}>
				  { showLog(this.state.msgLog) }
				</div> 
			</div>
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
