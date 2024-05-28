# Chorrespondence: a chord-based p2p mail network

## Goals

1. Create a generic chord-based DHT structure
2. Use the DHT to store a table messages 
3. Create a user interface to be able to send and recieve messages manually


### Specifics

- use a circular overlay network
- share a chatid with a user you want to chat with
- recieve messages by looking up the chat id + msg number: "chat123__1"


### The Physical Network

In order to limit the complexity of the network stack involved with this implementation, \
I will assume that all peers are on the same ipv6 network with different ports. Later I will try to get this to work \
with hosts on ipv6 networks with a firewall pinhole to allow this app specifically. \

This can be investigated later, but as far as I know, NAT traversal and holepunching are somewhat complex.


### The Message Table
key: 
  hashed chat_id + msg_index
  
value: 
  {
    chat_id
    msg_index
    msg_content
    sender_id
    timestamp
  }


### Implementation

- Single executable
- Has some commands
  - Init: for the first peer
  - Join address: joins the network

  - Leave: leaves the network
  - Run_store: Initiates a store operation to be carried out by the network
  - Run_lookup: Initiates a lookup operation to be carried out by the network

- Has some http endpoints
  - Store key value: adds a kv pair to the network if its not supposed to be stored in the current node
  - Lookup key: retrieves a kv pair from the network if its not currently stored in the current node
  - Get_pred: returns the pred of the current node
  - Update_pred sha: updates the pred of the current node

- Has some scheduled housekeeping
  - Stabilize
  - Notify
  - Fix_fingers

### Behavior
- Joining
  - Assume that there is a single node in the network already, with no succ or pred (N1)
  - Second node (N2):
    - Since N2 is joining it knows the N1's IP, set succ to N1. N2{succ=N1;pred=None} N1{succ=None;pred=None}
    - Asks for N1's pred and sets its pred to that. N2{succ=N1;pred=None} N1{succ=None;pred=None}
    - Tells N1 to update its pred to N2. N2{succ=N1;pred=None} N1{succ=None;pred=N2}
    - Stabilize: N2 asks N1 for its pred and decides if it is between N1 and N2 e.g. it should be N2's succ instead.
