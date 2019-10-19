# Make jar file with 'assembly' command in sbt

import os
import numpy as np
os.environ['CLASSPATH'] = "../../target/scala-2.13/Tetris-assembly-0.1.jar"


def smac_opt():
    # Import ConfigSpace and different types of parameters
    from smac.configspace import ConfigurationSpace
    from ConfigSpace.hyperparameters import UniformFloatHyperparameter
    # Import SMAC-utilities
    from smac.scenario.scenario import Scenario
    from smac.facade.smac_hpo_facade import SMAC4HPO

    n_params = 5

    def fun_to_optimize(x):
        from jnius import autoclass
        RunStrategy = autoclass('RunStrategy')

        params = [x[f'x{i}'] for i in range(0, n_params)]
        print(f'params:{params}')
        ret = -RunStrategy.runStrategyWithConfiguration(params, 20)
        print(ret)
        return ret

    cs = ConfigurationSpace()
    hyper_params = [UniformFloatHyperparameter(f"x{i}", 0, 100, default_value=1) for i in range(0, n_params)]
    cs.add_hyperparameters(hyper_params)

    # Scenario object
    scenario = Scenario({"run_obj": "quality",   # we optimize quality (alternatively runtime)
                         "runcount-limit": 999999,   # max. number of function evaluations; for this example set to a low number
                         "cs": cs,               # configuration space
                         "deterministic": "false"
                         })


    # Optimize, using a SMAC-object
    smac = SMAC4HPO(scenario=scenario,
                   rng=np.random.RandomState(42),
                   tae_runner=fun_to_optimize)

    smac.optimize()


if __name__ == '__main__':
    smac_opt()
