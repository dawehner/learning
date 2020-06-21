import React, { useState, useEffect } from 'react';
import { Text } from 'react-native'

// type Props = {
//   uri: string
// }

export default function PickFilterScreen({ route }) {
  const { uri } = route.params;
  console.log(arguments);
  return (
    <Text>the uri is: {uri}</Text>
  );
}