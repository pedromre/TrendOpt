package core;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import scala.collection.mutable.ListBuffer;

import org.jgap.FitnessFunction;
import org.jgap.IChromosome;
import org.ta4j.core.Indicator;

public class StrategyFitnessFunction extends FitnessFunction {
	
	private CandleSeries candles;
	private TimeSeriesLoader tsl;
	private Strategy strategy;
	private int initialCapital = 100000;
	//RSI
	private Indicator<?> rsiIndicator;
	private scala.collection.mutable.ListBuffer<Object> rsiBuffer;
	//MACD
	private Indicator<?> macdIndicator;
	private ListBuffer<Object> macdBuffer;
	//ROC
	private Indicator<?> rocIndicator;
	private ListBuffer<Object> rocBuffer;
	//Maps
	Map<Integer, Integer> ssmaMap;
	Map<Integer, Integer> lsmaMap;
	Map<Integer, ListBuffer<Object>> ssmaBufferMap;
	Map<Integer, ListBuffer<Object>> lsmaBufferMap;
	Map<Integer, ListBuffer<Object>> emaBufferMap;
	Map<Integer, Integer> rsiMap;
	Map<Integer, Integer> rocMap;
	Map<Integer, Double> macdMap;
	private int start;
	private int finish;
	private float BandH;
	
	public StrategyFitnessFunction(CandleSeries candles, TimeSeriesLoader tsl, int s, int f) {
		this.candles = candles;
		this.tsl = tsl;
		this.strategy = new Strategy(candles, "Strategy optimazation", initialCapital);
		start = s;
		finish = f;
		rsiIndicator = tsl.createRSI(14);
		rsiBuffer = tsl.convertTStoLB(rsiIndicator);
		//macdIndicator = tsl.
		
		rocIndicator = tsl.createROC(10);
		rocBuffer = tsl.convertTStoLB(rocIndicator);
		configureMaps();
	}
	
	@Override
	protected double evaluate(IChromosome chro) {
		strategy = new Strategy(candles, "Strategy optimazation", initialCapital);
		//strategy.resetStrategy();
		int up = rsiMap.get((Integer) chro.getGene(0).getAllele()); //(Integer) chro.getGene(0).getAllele();
		int down = rsiMap.get((Integer) chro.getGene(1).getAllele()); //(Integer) chro.getGene(1).getAllele();
		setRSIStrat(up,down);
		int s = ssmaMap.get((Integer) chro.getGene(2).getAllele());
		int l = s*lsmaMap.get((Integer) chro.getGene(3).getAllele());
		setSMACrossStrat((Integer) chro.getGene(2).getAllele(),l);
		//setEMACrossStrat((Integer) chro.getGene(4).getAllele(), (Integer) chro.getGene(5).getAllele());
		//long start = System.nanoTime();    
		strategy.simulateStrategy(start, finish); // candles.getSize()-600
		//long elapsedTime = System.nanoTime() - start;
		//System.out.println("Time: " + (double)elapsedTime /  1000000000.0);
		float roi = strategy.getROI();
		double fitness = roi + 1f;
		//strategy.getHistoric(0);
		//System.out.println("ROI:" + fitness + "ROIfloat:" + roi + "\t up:"+up+"down:"+down);
		return ((double)(fitness));
	}
	
	public float getBandH() {
		return BandH;
	}
	
	private void setRSIStrat(int up, int down) {
		strategy.setEntryRuleTestUnder(rsiBuffer, down);
		strategy.setExitRuleTestOver(rsiBuffer, up);
	}
	
	private void setSMACrossStrat(int shortSMAkey, int longSMAkey ) {
		strategy.setEntryRule(ssmaBufferMap.get(shortSMAkey), "", lsmaBufferMap.get(longSMAkey));
		strategy.setExitRule(lsmaBufferMap.get(longSMAkey), "", ssmaBufferMap.get(shortSMAkey));
	}
	
	private void setEMACrossStrat(int shortEMAkey, int longEMAkey ) {
		strategy.setEntryRule(emaBufferMap.get(shortEMAkey), "", emaBufferMap.get(longEMAkey));
		strategy.setExitRule(emaBufferMap.get(longEMAkey), "", emaBufferMap.get(shortEMAkey));
	}
	
	private void setROCStrat(int up_t, int down_t) {
		strategy.setEntryRuleTestUnder(rocBuffer, down_t);
		strategy.setExitRuleTestOver(rocBuffer, up_t);
	}

	private void configureMaps() {
		ssmaMap = new HashMap<Integer, Integer>();
		lsmaMap = new HashMap<Integer, Integer>();
		ssmaBufferMap = new HashMap<Integer, ListBuffer<Object>>();
		lsmaBufferMap = new HashMap<Integer, ListBuffer<Object>>();
		emaBufferMap = new HashMap<Integer, ListBuffer<Object>>();
		//ListBuffer<Object> shortSMAIndicator = tsl.convertTStoLB(tsl.createSMA(shortSMA));
		emaBufferMap.put(0, tsl.convertTStoLB(tsl.createSMA(5)));
		emaBufferMap.put(1, tsl.convertTStoLB(tsl.createSMA(10)));
		emaBufferMap.put(2, tsl.convertTStoLB(tsl.createSMA(15)));
		emaBufferMap.put(3, tsl.convertTStoLB(tsl.createSMA(20)));
		emaBufferMap.put(4, tsl.convertTStoLB(tsl.createSMA(30)));
		emaBufferMap.put(5, tsl.convertTStoLB(tsl.createSMA(50)));
		emaBufferMap.put(6, tsl.convertTStoLB(tsl.createSMA(80)));
		emaBufferMap.put(7, tsl.convertTStoLB(tsl.createSMA(100)));
		ssmaMap.put(0, 5);
		ssmaMap.put(1, 10);
		ssmaMap.put(2, 15);
		ssmaMap.put(3, 20);
		lsmaMap.put(0, 2);		
		lsmaMap.put(1, 3);
		lsmaMap.put(2, 4);
		lsmaMap.put(3, 5);
		
		for(int i = 0; i < ssmaMap.size(); i++) {
			ssmaBufferMap.put(i, tsl.convertTStoLB(tsl.createSMA(ssmaMap.get(i))));
			for(int j = 0; j < lsmaMap.size(); j++) {
				int tmp = ssmaMap.get(i) * lsmaMap.get(j);
				if(!lsmaBufferMap.containsKey(tmp)) {
					lsmaBufferMap.put(tmp, tsl.convertTStoLB(tsl.createSMA(tmp)));
				}
			}
		}
		
		
		rsiMap = new HashMap<Integer, Integer>();
		for(int i=0; i<=100; i = i+5) {
			rsiMap.put(i/5, i);
		}
		//rsiMap = maMap;
		
		rocMap = new HashMap<Integer, Integer>();
		rocMap.put(0, -20);
		rocMap.put(1, -18);
		rocMap.put(2, -16);
		rocMap.put(3, -14);
		rocMap.put(4, -12);
		rocMap.put(5, -10);
		rocMap.put(6, -8);
		rocMap.put(7, -6);
		rocMap.put(8, -4);
		rocMap.put(9, -2);
		rocMap.put(10, 0);
		rocMap.put(11, 2);
		rocMap.put(12, 4);
		rocMap.put(13, 6);
		rocMap.put(14, 8);
		rocMap.put(15, 10);
		rocMap.put(16, 12);
		rocMap.put(17, 14);
		rocMap.put(18, 16);
		rocMap.put(19, 18);
		rocMap.put(20, 20);
		
		macdMap = new HashMap<Integer, Double>();
		macdMap.put(0, -4.0);
		macdMap.put(1, -3.5);
		macdMap.put(2, -3.0);
		macdMap.put(3, -2.5);
		macdMap.put(4, -2.0);
		macdMap.put(5, -1.5);
		macdMap.put(6, -1.0);
		macdMap.put(7, -0.5);
		macdMap.put(8, 0.0);
		macdMap.put(9, 0.5);
		macdMap.put(10, 1.0);
		macdMap.put(11, 1.5);
		macdMap.put(12, 2.0);
		macdMap.put(13, 2.5);
		macdMap.put(14, 3.0);
		macdMap.put(15, 3.5);
		macdMap.put(16, 4.0);
	}
}
