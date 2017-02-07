package gui.views;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.plaf.metal.MetalComboBoxUI;

public class SteppedComboBox extends JComboBox {

	private class SteppedComboBoxUI extends MetalComboBoxUI {
		@Override
		protected ComboPopup createPopup() {
			BasicComboPopup popup = new BasicComboPopup(comboBox) {

				private static final long serialVersionUID = 1L;

				@Override
				public void show() {
					
					Dimension popupSize = ((SteppedComboBox) comboBox)
							.getPopupSize();
					popupSize.setSize(popupSize.width,
							getPopupHeightForRowCount(comboBox
									.getMaximumRowCount()));
					Rectangle popupBounds = computePopupBounds(0, comboBox
							.getBounds().height, popupSize.width,
							popupSize.height);
					scroller.setMaximumSize(popupBounds.getSize());
					scroller.setPreferredSize(popupBounds.getSize());
					scroller.setMinimumSize(popupBounds.getSize());
					list.invalidate();
					int selectedIndex = comboBox.getSelectedIndex();
					if (selectedIndex == -1) {
						list.clearSelection();
					} else {
						list.setSelectedIndex(selectedIndex);
					}
					list.ensureIndexIsVisible(list.getSelectedIndex());
					setLightWeightPopupEnabled(comboBox
							.isLightWeightPopupEnabled());

					show(comboBox, popupBounds.x, popupBounds.y);
				}
			};
			popup.getAccessibleContext().setAccessibleParent(comboBox);
			return popup;
		}
	}

	private static final long serialVersionUID = 1L;

	private int popupWidth;

	public SteppedComboBox() {
		super();
		setUI(new SteppedComboBoxUI());
		popupWidth = 0;
	}

	public SteppedComboBox(Vector<String> items) {
		super(items);
		setUI(new SteppedComboBoxUI());
		popupWidth = 0;
	}

	public Dimension getPopupSize() {
		Dimension size = this.getSize();
		if (popupWidth < 1) {
			popupWidth = size.width;
		}
		return new Dimension(popupWidth, size.height);
	}

	public void setPopupWidth(int i) {
		popupWidth = i;
	}

}
