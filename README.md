This repository contains a simplify version of a MAC Controller. Here are the constraints imposed:

* Protocole 802.3
* RGMII interface to talk with PHY (with a 125MHz clock)
* full-duplex communication
* Jumbo Frame not handle
* Asynchronous FIFO used for CDC purposes (1 at RGMII interface and 1 at System interface for MAC Tx and 1 at RGMII interface for MAC Rx)
* For MAC Tx :
* &nbsp;	preambule and SFD are written into RGMII FIFO as soon as it is empty
* &nbsp;	RGMII FIFO reading is enabled as long as the FIFO is not empty
* &nbsp;	System is authorized to write destination address, source address, length and payload into System FIFO if it starts the transfer with a start signal, until the end signal and as long as tx\_data\_valid is high
* &nbsp;	System FIFO contains bytes and can store a full frame
* &nbsp;	RGMII FIFO contains nibbles (word of 4 bits) and can store a full frame
* For MAC Rx :
* &nbsp;	FIFO can store a full frame
* &nbsp;	CRC is checked
* &nbsp;	nibble to byte conversion is done before storage into FIFO
* &nbsp;	System read directly from FIFO
