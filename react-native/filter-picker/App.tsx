import 'react-native-gesture-handler';
import React, { useState, useEffect } from 'react';
import { StyleSheet, Text, View, Button } from 'react-native';
import { NavigationContainer } from '@react-navigation/native';
import { createStackNavigator } from '@react-navigation/stack';

import PickImageScreen from './PickImageScreen';
import PickFilterScreen from './PickFilterScreen';

interface ImageSource {
  uri: String
};

const Stack = createStackNavigator();

export default function App() {
  return (
    <NavigationContainer>
      <Stack.Navigator>
        <Stack.Screen
          name="ImagePick"
          component={PickImageScreen}
          options={{ title: 'Pick image' }}
        />
        <Stack.Screen
          name="FilterPick"
          component={PickFilterScreen}
          options={{ title: 'Pick filter' }}
        />
      </Stack.Navigator>
    </NavigationContainer>
  );
}
