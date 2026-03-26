
from pcvs.plugins import Plugin


class MPIPlugin(Plugin):
    step = Plugin.Step.TEST_EVAL

    def run(self, *args, **kwargs):
        # returns True if the combination should be used
        config = kwargs['config']
        nb_nodes = config['machine'].get('nodes', 1)
        nb_cores = config['machine'].get('cores_per_node', 1)

        comb = kwargs['combination']
        n_mpi = comb.get('n_mpi', 1)
        # n_omp = comb.get('n_omp', 0)
        n_node = comb.get('n_node', 1)

        if ((int(n_mpi / n_node) > nb_cores) or (n_node > nb_nodes)):
            return False
        else:
            return True

