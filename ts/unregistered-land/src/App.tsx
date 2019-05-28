import React, { useEffect, useState } from 'react';
import parse from 'csv-parse/lib/sync';

import './App.css';
import { map } from 'ramda';

type CsvRow = {
  department: string;
  landArea: number;
};

function App() {
  const [data, setData] = useState<null | Array<CsvRow>>(null);
  useEffect(() => {
    const fetchData = async () => {
      const result = await fetch('/unregistered.csv');
      const parsedCsv = parse(await result.text(), {
        columns: true,
      });
      const convertedData = map((r: { [s: string]: string }) => ({
        department: r.Department,
        landArea: parseFloat(r.LandAreaHA),
      }))(parsedCsv);

      setData(convertedData);
    };
    if (!data) {
      fetchData();
    }
  }, [data]);

  return (
    data && (
      <ul>
        {data.map(row => (
          <li>{row.landArea}</li>
        ))}
      </ul>
    )
  );
}

export default App;
