import React from 'react';

const Repos = ( { resource  }) => {
  return <div>{ resource.repos.read().length }</div>
};

export default Repos;