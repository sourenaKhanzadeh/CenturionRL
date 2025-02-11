// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SampleContract {
    uint256 public count;

    event Increment(address indexed sender, uint256 newCount);

    constructor() {
        count = 0;
    }

    function increment() public {
        count += 1;
        emit Increment(msg.sender, count);
    }

    function getCount() public view returns (uint256) {
        return count;
    }
}
