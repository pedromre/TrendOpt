package core;

import org.jfree.chart.*;
import org.jfree.chart.axis.*;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.CandlestickRenderer;
import org.jfree.data.xy.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.*;
import java.net.URL;
import java.text.*;
import java.util.*;
import java.util.List;


public class CandlestickChart extends JFrame {

	public void CloseCandlestickChart() {
		
		this.dispatchEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	};
	
    public CandlestickChart(CandleSeries stock) {
        super("CandlestickDemo2");
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        final DateAxis domainAxis = new DateAxis("Date");
        NumberAxis rangeAxis = new NumberAxis("Price");
        CandlestickRenderer renderer = new CandlestickRenderer();
        XYDataset dataset = getDataSet(stock);
        XYPlot mainPlot = new XYPlot(dataset, domainAxis, rangeAxis, renderer);
        //Do some setting up, see the API Doc
        renderer.setSeriesPaint(0, Color.BLACK);
        renderer.setDrawVolume(false);
        rangeAxis.setAutoRangeIncludesZero(false);
        //Now create the chart and chart panel
        JFreeChart chart = new JFreeChart(stock.toString(), null, mainPlot, false); /////////////// Modify
        ChartPanel chartPanel = new ChartPanel(chart, false);
        chartPanel.setPreferredSize(new Dimension(600, 300));
        mainPlot.setDomainPannable(true);
        mainPlot.setRangePannable(true);
        this.add(chartPanel);
        // Add tiemline toggle
        final Timeline oldTimeline = domainAxis.getTimeline();
        final Timeline newTimeline = SegmentedTimeline.newMondayThroughFridayTimeline();
        this.add(new JCheckBox(new AbstractAction("Segmented Timeline") {
            @Override
            public void actionPerformed(ActionEvent e) {
                JCheckBox jcb = (JCheckBox) e.getSource();
                if (jcb.isSelected()) {
                    domainAxis.setTimeline(newTimeline);
                } else {
                    domainAxis.setTimeline(oldTimeline);
                }
            }
        }), BorderLayout.SOUTH);
        this.pack();
        this.setLocationRelativeTo(null);
    }

    private AbstractXYDataset getDataSet(CandleSeries stock) {
        //This is the dataset we are going to create
        DefaultOHLCDataset result;
        //This is the data needed for the dataset
        OHLCDataItem[] data;
        //This is where we go get the data, replace with your own data source
        data = getData(stock);
        //Create a dataset, an Open, High, Low, Close dataset
        result = new DefaultOHLCDataset(stock.toString(), data); //////////////////////////////////////Modify
        return result;
    }
    //This method uses yahoo finance to get the OHLC data

    protected OHLCDataItem[] getData(CandleSeries stock) {
        List<OHLCDataItem> dataItems = new ArrayList<OHLCDataItem>();
        
            DateFormat df = new SimpleDateFormat("y-M-d");
            
            for(int i = 0; i < stock.CS().length(); ++i) {
            	Date date = stock.getCandleDate(i);
                double open = stock.getCandleOpen(i);
                double high = stock.getCandleHigh(i);
                double low = stock.getCandleLow(i);
                double close = stock.getCandleClose(i);
                double volume = stock.getCandleVolume(i);
            	OHLCDataItem item = new OHLCDataItem(date, open, high, low, close, volume);
            	dataItems.add(item);
            }
            
            
        
        //Data from Yahoo is from newest to oldest. Reverse so it is oldest to newest
        //Collections.reverse(dataItems);
        //Convert the list into an array
        OHLCDataItem[] data = dataItems.toArray(new OHLCDataItem[dataItems.size()]);
        return data;
    }

    public static void main(String[] args) {
        EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                //new CandlestickChart().setVisible(true);
            }
        });
    }
}
