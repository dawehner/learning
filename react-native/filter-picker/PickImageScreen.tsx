import React, { useState, useEffect } from 'react';
import { StyleSheet, Text, View, Button } from 'react-native';
import ImagePicker from 'react-native-image-picker';
import ImageResizer from 'react-native-image-resizer';

interface ImageSource {
  uri: String
};

export default function PickImageScreen({ navigation }) {
  const onPickImageButton = () => {
    // More info on all the options is below in the API Reference... just some common use cases shown here
    const options = {
      title: 'Select Avatar',
      customButtons: [{ name: 'fb', title: 'Choose Photo from Facebook' }],
      storageOptions: {
        skipBackup: true,
        path: 'images',
      },
    };

    /**
     * The first arg is the options object for customization (it can also be null or omitted for default options),
     * The second arg is the callback which sends object: response (more info in the API Reference)
     */
    ImagePicker.showImagePicker(options, (response) => {
      // console.log('Response = ', response);

      if (response.didCancel) {
        console.log('User cancelled image picker');
      } else if (response.error) {
        console.log('ImagePicker Error: ', response.error);
      } else if (response.customButton) {
        console.log('User tapped custom button: ', response.customButton);
      } else {
        console.log({ ImageResizer })
        ImageResizer.createResizedImage(response.uri, 320, 320, 'JPEG', 100)
          .then(response2 => {
            // You can also display the image using data:
            // const source = { uri: 'data:image/jpeg;base64,' + response.data };

            navigation.navigate('PickFilter', { uri: response2.uri, fullUri: response.uri })
          })
          .catch(console.error)
      }
    });
  }


  return (
    <View style={styles.container}>
      <Button onPress={onPickImageButton}
        title="Pick"
        accessibilityLabel="Pick an image to choose a filter for"
      ></Button>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'center',
  },
});
