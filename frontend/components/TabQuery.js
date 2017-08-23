import React, { Component } from 'react';

function showResult(json) {
  if (json != null && json.msg == "query-result") {
  return (
    <div>
      <h5>Raw Result:</h5> 
      {JSON.stringify(json.para)}
      <h5>Table Result:</h5> 
    </div>
    );
  } else {
    return (<div>no result</div>);
  }
}

class TabQuery extends React.Component {
  // render function 
  render() {
    return (
    <div>
    <div className="row">
      <div className="col-sm-12">
        <h3><small>Query</small></h3>
      </div>
    </div>
    <div className="row">
      <div className="form-group col-sm-8">
        <input style={{width: 500 + "px"}} type="text" className="form-control" id="query"></input>
      </div>
      <div className="col-sm-4">
        <button className="btn btn-primary btn-sm" onClick={ () => {
          var cmd = $('#query')[0].value;
          this.props.sbc.callShoebox("query", cmd);
        }}>
        Query DB
        </button>
      </div>
    </div>

    <div className="row">
      <div className="col-sm-12">
        <h3><small>Query Result</small></h3>
        {showResult(this.state.jsonResult)}
      </div>
    </div>

  </div>
      );  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { jsonResult : null };
    this.update = this.update.bind(this);
    }

    update(e, d) {
      this.setState({ jsonResult : d}); 
    }

    componentDidMount() {
      this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
      this.props.sbc.unsubscribe(this.update);
    } 

}

export default TabQuery; 