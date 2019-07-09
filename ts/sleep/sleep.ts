interface DelayTime {
  type: "delaytime";
  // Delay in milliseconds
  amount: number;
}

interface DelayFunction {
  type: "delayfunction";
  condition: () => boolean;
}

export const milliseconds = (time: number) : Delay => {
  return {
    type: "delaytime",
    amount: time,
  };
};

export const tillSuccess = (condition: () => boolean) : Delay => {
  return {
    type: "delayfunction",
    condition,
  };
};

export const start = (delay : Delay) : Promise<void> => {
  return new Promise<void>((res: { (): void; (): void; }) => {
    switch (delay.type) {
      case "delayfunction":
        const intervalId = setInterval(() => {
          if (delay.condition()) {
            clearInterval(intervalId);
            return res();
          }
        }, 10)
        break;
      case "delaytime":
        setTimeout(() => {
          return res();
        }, delay.amount);
        break;
    }
  });
};
  
export type Delay = DelayTime | DelayFunction