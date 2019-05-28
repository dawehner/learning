import React, { useEffect, useState } from 'react';
import Octokit from '@octokit/rest';
import OctoApp from '@octokit/app';
import './App.css';
import * as env from env;

function App() {
  const [myIssues, setIssues] = useState(null);

  useEffect(() => {
    async function fetchData() {

    const APP_ID = env.get('APP_ID');
    const PRIVATE_KEY = env.get('APP_PRIVATE_KEY');

      const app = new OctoApp({ id: APP_ID, privateKey: PRIVATE_KEY })

      const installationToken = await app.getInstallationAccessToken({
        installationId: '889541',
      });

      const octokit = new Octokit();

      octokit.authenticate({
        type: 'token',
        token: installationToken,
      });

      try {
        const issues = await octokit.issues.listForRepo({
          owner: 'THE-Engineering',
          repo: 'tech-standards',
        });

        setIssues(issues);
      }
      catch (e) {
        console.error(e);
      }
    }

    fetchData();
  });

  return (
    myIssues && <div><ul>{myIssues.data.map(issue => (<li>
      <a href={issue.url}>{issue.title}</a>
    </li>))}</ul></div>
  );
}

export default App;
