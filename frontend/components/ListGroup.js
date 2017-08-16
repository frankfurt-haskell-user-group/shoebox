import React, { Component } from 'react';

function listGroup() {
	const listItems = this.props.items.map(function (i) {
		if (i != this.state.active) {
			return <button className="list-group-item" type="button" onClick={
				function () {
					this.setState({active : i});
					this.props.onchange(i);
				}.bind(this)
			}>{i}</button>
		} else {
			return <button className="list-group-item active" type="button">{i}</button>
		}
	}.bind(this));
	return (
		<div className="list-group">
			{ listItems }
		</div>
		);
	}

class ListGroup extends React.Component {
	
	render() {
		return listGroup.bind(this)();
	}	

	constructor(props) {
		super(props);
		this.state = { active : null };	
	}
}

export default ListGroup; 