/*
  NFT for Event Attendance
*/
import Cycles "mo:base/ExperimentalCycles";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import AID "./motoko/util/AccountIdentifier";
import ExtCore "./motoko/ext/Core";
import ExtCommon "./motoko/ext/Common";
import ExtAllowance "./motoko/ext/Allowance";
import ExtNonFungible "./motoko/ext/NonFungible";

import Text "mo:base/Text";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import List "mo:base/List";
import Blob "mo:base/Blob";
import Buffer "mo:base/Buffer";

shared (install) actor class attendance_nft() = this {

  // Types
  type AccountIdentifier = ExtCore.AccountIdentifier;
  type SubAccount = ExtCore.SubAccount;
  type User = ExtCore.User;
  type Balance = ExtCore.Balance;
  type TokenIdentifier = ExtCore.TokenIdentifier;
  type TokenIndex = ExtCore.TokenIndex;
  type Extension = ExtCore.Extension;
  type CommonError = ExtCore.CommonError;
  type BalanceRequest = ExtCore.BalanceRequest;
  type BalanceResponse = ExtCore.BalanceResponse;
  type TransferRequest = ExtCore.TransferRequest;
  type TransferResponse = ExtCore.TransferResponse;
  type AllowanceRequest = ExtAllowance.AllowanceRequest;
  type ApproveRequest = ExtAllowance.ApproveRequest;
  type Metadata = ExtCommon.Metadata;
  type MintRequest = ExtNonFungible.MintRequest;

  type EVENTID = Nat;

  type EventCollection = {
    eid : EVENTID;
    title : Text;
    datetime : Int;
    location : Text;
    host : Text;
    asset : Text;
    claimcode : Text;
    status : {
      #new;
      #ready;
      #close;
    };
  };

  type Inventory = {
    token : TokenIndex;
    event : EventCollection;
  };

  type SearchCritera = {
    #eventid : EVENTID;
    #owner : {
      #principal : Principal;
      #address : AccountIdentifier;
    };
  };

  private let EXTENSIONS : [Extension] = ["@ext/common", "@ext/allowance", "@ext/nonfungible"];

  //State work
  private stable var _registryState : [(TokenIndex, AccountIdentifier)] = [];
  private var _registry : HashMap.HashMap<TokenIndex, AccountIdentifier> = HashMap.fromIter(_registryState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

  private stable var _allowancesState : [(TokenIndex, Principal)] = [];
  private var _allowances : HashMap.HashMap<TokenIndex, Principal> = HashMap.fromIter(_allowancesState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

  private stable var _tokenMetadataState : [(TokenIndex, Metadata)] = [];
  private var _tokenMetadata : HashMap.HashMap<TokenIndex, Metadata> = HashMap.fromIter(_tokenMetadataState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

  private stable var _eventMintAllowState : [(EVENTID, [Principal])] = [];
  private var _eventMintAllow : HashMap.HashMap<EVENTID, [Principal]> = HashMap.fromIter(_eventMintAllowState.vals(), 0, Nat.equal, Hash.hash);

  private stable var _eventCollectionState : [(EVENTID, EventCollection)] = [];
  private var _eventCollections : HashMap.HashMap<EVENTID, EventCollection> = HashMap.fromIter(_eventCollectionState.vals(), 0, Nat.equal, Hash.hash);

  private stable var _eventAttendanceState : [(EVENTID, [Principal])] = [];
  private var _eventAttendance : HashMap.HashMap<EVENTID, [Principal]> = HashMap.fromIter(_eventAttendanceState.vals(), 0, Nat.equal, Hash.hash);

  private stable var _inventoryState : [(Principal, [Inventory])] = [];
  private var _inventory : HashMap.HashMap<Principal, [Inventory]> = HashMap.fromIter(_inventoryState.vals(), 0, Principal.equal, Principal.hash);

  private stable var _supply : Balance = 0;
  private stable var _minter : Principal = install.caller;
  private stable var _minters : [Principal] = [];
  private var minters : List.List<Principal> = List.fromArray(_minters);
  private stable var _nextTokenId : TokenIndex = 0;

  //State functions
  system func preupgrade() {
    _registryState := Iter.toArray(_registry.entries());
    _allowancesState := Iter.toArray(_allowances.entries());
    _tokenMetadataState := Iter.toArray(_tokenMetadata.entries());

    _minters := List.toArray(minters);
    _eventCollectionState := Iter.toArray(_eventCollections.entries());
    _eventAttendanceState := Iter.toArray(_eventAttendance.entries());

    _inventoryState := Iter.toArray(_inventory.entries());

  };

  system func postupgrade() {
    _registryState := [];
    _allowancesState := [];
    _tokenMetadataState := [];

    _minters := [];
    _eventCollectionState := [];
    _eventAttendanceState := [];
    _inventoryState := [];
  };

  public shared (msg) func setMinter(minter : Principal) : async () {
    assert (msg.caller == _minter);
    _minter := minter;
  };

  public shared (msg) func addMinter(minter : Principal) : async () {
    assert (msg.caller == _minter);
    minters := List.push(minter, minters);
  };

  public query ({ caller }) func isMinter() : async Bool {
    let minter = List.find(
      minters,
      func(m : Principal) : Bool {
        m == caller;
      },
    );

    switch (minter) {
      case (?minter) { true };
      case (_) { false };

    };
  };

  public shared (msg) func mintNFT(request : MintRequest) : async TokenIndex {
    assert (msg.caller == _minter);
    _mint(request);
  };

  public shared (msg) func mintANFT(event : EventCollection) : async Result.Result<Nat, Text> {
    // let minter = List.find(
    //   minters,
    //   func(m : Principal) : Bool {
    //     m == msg.caller;
    //   },
    // );

    // switch (minter) {
    //   case (?minter) {
        _eventCollections.put(event.eid, event);
        #ok(1);
    //   };
    //   case (_) {
    //     #err("Not allow to mint");
    //   };
    // };

  };

  public shared (msg) func checkANFT(eid : Nat) : async ?EventCollection {
    _eventCollections.get(eid);
  };

  public shared (msg) func openClaim(eid : EVENTID) : async Result.Result<Nat, Text> {
    if (Principal.isAnonymous(msg.caller)) {
      #err("no authenticated");
    } else {
      let event = _eventCollections.get(eid);
      switch (event) {
        case (?event) {
          _eventCollections.put(
            eid,
            {
              eid = eid;
              title = event.title;
              datetime = event.datetime;
              location = event.location;
              host = event.host;
              asset = event.asset;
              claimcode = event.claimcode;
              status = #ready;
            },
          );
          #ok(1);
        };
        case (_) {
          #err("no event collection found");
        };
      };
    };
  };

  public shared (msg) func closeClaim(eid : EVENTID) : async Result.Result<Nat, Text> {
    if (Principal.isAnonymous(msg.caller)) {
      #err("no authenticated");
    } else {
      let event = _eventCollections.get(eid);
      switch (event) {
        case (?event) {
          _eventCollections.put(
            eid,
            {
              eid = eid;
              title = event.title;
              datetime = event.datetime;
              location = event.location;
              host = event.host;
              asset = event.asset;
              claimcode = event.claimcode;
              status = #close;
            },
          );
          #ok(1);
        };
        case (_) {
          #err("no event collection found");
        };
      };
    };
  };

  public shared (msg) func claimANFT(eid : EVENTID, code : Text) : async Result.Result<TokenIndex, Text> {
    if (Principal.isAnonymous(msg.caller)) {
      #err("no authenticated");
    } else {
      let event = _eventCollections.get(eid);
      switch (event) {
        case (?event) {
          if (event.status == #ready) {

            if (event.claimcode == code) {
              let isAttend = _attend(eid, msg.caller);
              if (isAttend) {
                let tokenIndex = _mint({
                  to = #principal(msg.caller);
                  metadata = ?Text.encodeUtf8(event.asset);
                });
                //save inventory
                let userInventory = _inventory.get(msg.caller);
                switch (userInventory) {
                  case (?userInventory) {
                    let pb = Buffer.Buffer<Inventory>(userInventory.size());
                    for (p in userInventory.vals()) {
                      pb.add(p);
                    };
                    pb.add({
                      token = tokenIndex;
                      event = event;
                    });
                    _inventory.put(msg.caller, pb.toArray());
                  };
                  case (_) {
                    let pb = Buffer.Buffer<Inventory>(0);
                    pb.add({
                      token = tokenIndex;
                      event = event;
                    });
                    _inventory.put(msg.caller, pb.toArray());
                  };
                };

                #ok(tokenIndex);

              } else {
                #err("you already claimed!");
              };

            } else {
              #err("wrong code");
            };
          } else {
            #err("NFT is not ready to mint");
          };
        };
        case (_) {
          #err("no event collection found");
        };
      };
    };

  };

  public query (msg) func inventory() : async [Inventory] {
    let ins = _inventory.get(msg.caller);
    switch (ins) {
      case (?ins) { ins };
      case (_)[];
    };

  };

  public shared (msg) func transfer(request : TransferRequest) : async TransferResponse {
    if (request.amount != 1) {
      return #err(#Other("Must use amount of 1"));
    };
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
      return #err(#InvalidToken(request.token));
    };
    let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let owner = ExtCore.User.toAID(request.from);
    let spender = AID.fromPrincipal(msg.caller, request.subaccount);
    let receiver = ExtCore.User.toAID(request.to);

    switch (_registry.get(token)) {
      case (?token_owner) {
        if (AID.equal(owner, token_owner) == false) {
          return #err(#Unauthorized(owner));
        };
        if (AID.equal(owner, spender) == false) {
          switch (_allowances.get(token)) {
            case (?token_spender) {
              if (Principal.equal(msg.caller, token_spender) == false) {
                return #err(#Unauthorized(spender));
              };
            };
            case (_) {
              return #err(#Unauthorized(spender));
            };
          };
        };
        _allowances.delete(token);
        _registry.put(token, receiver);
        return #ok(request.amount);
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };

  public shared (msg) func approve(request : ApproveRequest) : async () {
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
      return;
    };
    let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let owner = AID.fromPrincipal(msg.caller, request.subaccount);
    switch (_registry.get(token)) {
      case (?token_owner) {
        if (AID.equal(owner, token_owner) == false) {
          return;
        };
        _allowances.put(token, request.spender);
        return;
      };
      case (_) {
        return;
      };
    };
  };

  public query func getMinter() : async Principal {
    _minter;
  };
  public query func extensions() : async [Extension] {
    EXTENSIONS;
  };

  public query func balance(request : BalanceRequest) : async BalanceResponse {
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
      return #err(#InvalidToken(request.token));
    };
    let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let aid = ExtCore.User.toAID(request.user);
    switch (_registry.get(token)) {
      case (?token_owner) {
        if (AID.equal(aid, token_owner) == true) {
          return #ok(1);
        } else {
          return #ok(0);
        };
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };

  public query func allowance(request : AllowanceRequest) : async Result.Result<Balance, CommonError> {
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
      return #err(#InvalidToken(request.token));
    };
    let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let owner = ExtCore.User.toAID(request.owner);
    switch (_registry.get(token)) {
      case (?token_owner) {
        if (AID.equal(owner, token_owner) == false) {
          return #err(#Other("Invalid owner"));
        };
        switch (_allowances.get(token)) {
          case (?token_spender) {
            if (Principal.equal(request.spender, token_spender) == true) {
              return #ok(1);
            } else {
              return #ok(0);
            };
          };
          case (_) {
            return #ok(0);
          };
        };
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };

  public query func bearer(token : TokenIdentifier) : async Result.Result<AccountIdentifier, CommonError> {
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
      return #err(#InvalidToken(token));
    };
    let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_registry.get(tokenind)) {
      case (?token_owner) {
        return #ok(token_owner);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
  };

  public query func supply(token : TokenIdentifier) : async Result.Result<Balance, CommonError> {
    #ok(_supply);
  };

  public query func getRegistry() : async [(TokenIndex, AccountIdentifier)] {
    Iter.toArray(_registry.entries());
  };
  public query func getAllowances() : async [(TokenIndex, Principal)] {
    Iter.toArray(_allowances.entries());
  };
  public query func getTokens() : async [(TokenIndex, Metadata)] {
    Iter.toArray(_tokenMetadata.entries());
  };

  public query func metadata(token : TokenIdentifier) : async Result.Result<Metadata, CommonError> {
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
      return #err(#InvalidToken(token));
    };
    let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_tokenMetadata.get(tokenind)) {
      case (?token_metadata) {
        return #ok(token_metadata);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
  };

  //Internal cycle management - good general case
  public func acceptCycles() : async () {
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  };
  public query func availableCycles() : async Nat {
    return Cycles.balance();
  };

  //------------------ private methods ----------------------------------
  private func _mint(request : MintRequest) : TokenIndex {
    let receiver = ExtCore.User.toAID(request.to);
    let token = _nextTokenId;
    let md : Metadata = #nonfungible({
      metadata = request.metadata;
    });
    _registry.put(token, receiver);
    _tokenMetadata.put(token, md);
    _supply := _supply + 1;
    _nextTokenId := _nextTokenId + 1;
    token;
  };

  private func _attend(eid : EVENTID, principal : Principal) : Bool {
    let ps = _eventAttendance.get(eid);
    switch (ps) {
      case (?ps) {
        let fp = Array.find<Principal>(
          ps,
          func(p : Principal) : Bool {
            p == principal;
          },
        );
        switch (fp) {
          case (?fp) {
            false;
          };
          case (_) {
            let pb = Buffer.Buffer<Principal>(ps.size());
            for (p in ps.vals()) {
              pb.add(p);
            };
            pb.add(principal);
            _eventAttendance.put(eid, pb.toArray());
            true;
          };
        };

      };
      case (_) {
        _eventAttendance.put(eid, [principal]);
        true;
      };
    };
  };
};
