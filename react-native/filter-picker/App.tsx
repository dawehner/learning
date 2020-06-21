import 'react-native-gesture-handler';
import React, { useState, useEffect } from 'react';
import { StyleSheet, Text, View, Button } from 'react-native';
import { NavigationContainer } from '@react-navigation/native';
import { createStackNavigator } from '@react-navigation/stack';

import PickImageScreen from './PickImageScreen';
import PickFilterScreen from './PickFilterScreen';

const Stack = createStackNavigator<RootStackParamList>();

export default function App() {
  return (
    <NavigationContainer>
      <Stack.Navigator initialRouteName="PickImage">
        <Stack.Screen
          name="PickImage"
          component={PickImageScreen}
          options={{ title: 'Pick image' }}
        />
        <Stack.Screen
          name="PickFilter"
          component={PickFilterScreen}
          options={{ title: 'Pick filter' }}
        />
      </Stack.Navigator>
    </NavigationContainer>
  );
}
