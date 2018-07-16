package core;

import java.util.Hashtable;
import java.util.Map;

import org.jgap.*;
import org.jgap.Configuration;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.InvalidConfigurationException;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.IntegerGene;
import org.jgap.impl.*	;

public class GeneticAlgorithm {
static int a_sizeOfPopulation = 100; //50
	
	private CandleSeries candles;
	private TimeSeriesLoader tsl;
	private float BandH;
	private int start;
	private int finish;
	
	public GeneticAlgorithm(CandleSeries candles, TimeSeriesLoader tsl, int s, int f) {
		this.candles = candles;
		this.tsl = tsl;
		start = s;
		finish = f;
	}

	public Genotype configureJGAP() throws InvalidConfigurationException {
		Configuration gaConf = new DefaultConfiguration();
		gaConf.setPopulationSize(a_sizeOfPopulation);
		gaConf.setPreservFittestIndividual(true);
		//gaConf.setKeepPopulationSizeConstant(true);
		
		StrategyFitnessFunction fitnessFunc = new StrategyFitnessFunction(candles, tsl, start, finish);
		gaConf.setFitnessFunction(fitnessFunc);
		
		BandH = fitnessFunc.getBandH();
		
		Gene[] sampleGenes = configureGenes(gaConf);
		
		Chromosome sampleChromosome = new Chromosome(gaConf, sampleGenes);
		
		gaConf.setSampleChromosome(sampleChromosome);
		
		Genotype population = Genotype.randomInitialGenotype(gaConf);
		
		return population;
	}
	
	private Gene[] configureGenes(Configuration gaConf) throws InvalidConfigurationException {
		Gene[] sampleGenes = new Gene[4];
		sampleGenes[0] = new IntegerGene(gaConf,0,20);
		sampleGenes[1] = new IntegerGene(gaConf,0,20);
		sampleGenes[2] = new IntegerGene(gaConf,0,3);
		sampleGenes[3] = new IntegerGene(gaConf,0,3);
//		sampleGenes[4] = new IntegerGene(gaConf,0,7);
//		sampleGenes[5] = new IntegerGene(gaConf,0,7);
		
		return sampleGenes;
	}
	
	public float getBandH() {
		return BandH;
	}
	
}
