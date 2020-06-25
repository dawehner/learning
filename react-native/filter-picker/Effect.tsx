import React from 'react'

// Data Effect
import { EFFECT_OPTIONS, LIST_COMPONENTS } from './DataEffect'

type Props = {
  effect: string;
  children: Element;
}

export default function Effect({ children, effect }: Props) {
  const effectOptions = EFFECT_OPTIONS.filter(effectOption => effectOption.filter === effect);
  const effectOption = effectOptions.shift();

  if (!effectOption) {
    return children;
  }

  const EffectComponent = LIST_COMPONENTS[effectOption.id];

  return EffectComponent ? (<EffectComponent image={children} />) : null;
}