import React, { Suspense } from 'react';

import Repos from './Repos';
import fetchResource from './api';

const resource = fetchResource();

const App = () => {
  return <Suspense fallback={<h1>Loading...</h1>}>
    <Repos resource={resource} />
  </Suspense>;
};

export default App;
