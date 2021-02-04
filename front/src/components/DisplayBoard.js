import React from 'react'

export const DisplayBoard = ({numberOfItems, getAllItems}) => {

    return(
        <div className="display-board">
            <h4>Items Created</h4>
            <div className="number">
                {numberOfItems}
            </div>
            <div className="btn">
                <button type="button" onClick={(e) => getAllItems()} className="btn btn-warning">Get all Items</button>
            </div>
        </div>
    )
}