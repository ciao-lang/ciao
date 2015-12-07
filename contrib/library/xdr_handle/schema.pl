schema([ xmlns = "urn:schemas-microsoft-com:xml-data",
         'xmlns:dt' = "urn:schemas-microsoft-com:datatypes"
       ],
       [
         attribute_type("product_name",  
                        ['dt:type' = "string"
                        ]
                       ),
         attribute_type("currency", 
                        ['dt:type' = "enumeration",
                         'dt:values' = "dollar euro mark peseta franc"
                        ]
                       )
       ],
       [ 
         element_type("product",
                      [ content = "eltOnly",
                        model="closed"
                      ],
                      [],
                      [],
                      [ attribute("product_name", 
                                  [required = "yes"])
                      ],
                      [ element("currently", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("quantity",
                                [maxOccurs="1", minOccurs="1"]),
                        element("time-left", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("first-bid", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("number-of-bids",
                                [maxOccurs="1", minOccurs="1"]),
                        element("location",
                                [maxOccurs="1", minOccurs="1"]),
                        element("started", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("ends", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("author", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("high-bid", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("payment", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("shipping", 
                                [maxOccurs="1", minOccurs="1"]),
                        element("negotiation",  
                                [maxOccurs="1", minOccurs="1"]),
                        element("description", 
                                [maxOccurs="1", minOccurs="1"])
                      ]
                     ),
         element_type("currently", 
                      ['dt:type'="float"],
                      %% <description>Actual product price.</description>
                      [],
                      [],
                      [attribute("currency", [required="yes"])],
                      []
                     )
       ],
       [],
       [element("product", [maxOccurs="1", minOccurs="1"])]
      ). %% Closes schema_node.
        
       
 %%         <ElementType name="quantity" dt:type="i1">
 %%                 <description>Quantity offered/demanded</description>
 %%         </ElementType>
 %%         <ElementType name="time-left" dt:type="dateTime">
 %%                 <description>Time to remove the product</description>
 %%         </ElementType>
 %%         <ElementType name="first-bid" dt:type="float">
 %%                 <description>First bid in the auction</description>
 %%         </ElementType>
 %%         <ElementType name="number-of-bids" dt:type="i1">
 %%                 <description>Number of realized bids</description>
 %%         </ElementType>
 %%         <ElementType name="location" dt:type="string">
 %%                 <description>Location of the seller</description>
 %%         </ElementType>
 %%         <ElementType name="started" dt:type="date">
 %%                 <description>Started date</description>
 %%         </ElementType>
 %%         <ElementType name="ends" dt:type="date">
 %%                 <description>End date</description>
 %%         </ElementType>
 %%         <ElementType name="author" dt:type="string">
 %%                 <description>Author data</description>
 %%         </ElementType>
 %%         <ElementType name="high-bid" dt:type="float">
 %%                 <description>Higer bid</description>
 %%         </ElementType>
 %%         <ElementType name="payment" dt:type="string">
 %%                 <description>Method of payment</description>
 %%         </ElementType>
 %%         <ElementType name="shipping" dt:type="float">
 %%                 <description>Method of delivery</description>
 %%         </ElementType>
 %%         <ElementType name="description" content="eltOnly">
 %%                 <description>Product description</description>
 %%                 <element type="type" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="family" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="Specie" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="Size" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="Quality" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="ProductPresentation"
 %%                          maxOccurs="1" minOccurs="1"/>
 %%         </ElementType>
 %% 
 %%         <AttributeType name="type_values" 
 %%                        dt:type="enumeration"
 %%                        dt:values="offer demand"/>
 %%         <ElementType name="type" content="eltOnly">
 %%                 <description>Petition's type (offer/demand)</description>
 %%                 <attribute type="type_values" required="yes"/>
 %%         </ElementType>
 %% 
 %%         <AttributeType name="family_values"
 %%                        dt:type="enumeration"
 %%                        dt:values=
 %%              "Le_poisson_bleu  White_fish Cephalopods Crustaceans Shellfish"/>
 %%         <ElementType name="family" content="eltOnly">
 %%                 <description>Fish's family</description>
 %%                 <attribute type="family_values" required="yes"/>
 %%         </ElementType>
 %% 
 %%         <ElementType name="Specie" dt:type="string">
 %%                 <description>Fish's specie</description>
 %%         </ElementType>
 %% 
 %%         <ElementType name="Size" dt:type="i1">
 %%                 <description>Fish's size</description>
 %%         </ElementType>
 %% 
 %%         <AttributeType name="Quality_values"
 %%                        dt:type="enumeration"
 %%                        dt:values="A B C E"/>
 %%         <ElementType name="Quality" content="eltOnly">
 %%                 <description>Fish's quality</description>
 %%                 <attribute type="Quality_values" required="yes"/>
 %%         </ElementType>
 %% 
 %% 
 %%         <AttributeType name="ProductPresentation_values"
 %%                        dt:type="enumeration"
 %%                        dt:values=
 %%                      "Whole Gutted No head gutted Transformed Ready-Cooked"/>
 %%         <ElementType name="ProductPresentation" content="eltOnly">
 %%                 <description>Fish's presentation</description>
 %%                 <attribute type="ProductPresentation_values" required="yes"/>
 %%         </ElementType>
 %%         <ElementType name="negotiation" content="eltOnly">
 %%                 <description>Negotiation preferences</description>
 %%                 <element type="preference" maxOccurs="*" minOccurs="0"/>
 %%         </ElementType>
 %%         <ElementType name="preference" content="eltOnly">
 %%                 <description>Preferences data</description>
 %%                 <element type="constraint" maxOccurs="1" minOccurs="1"/>
 %%                 <element type="price" maxOccurs="1" minOccurs="1"/>
 %%         </ElementType>
 %%         <ElementType name="constraint" content="textOnly">
 %%                 <description>Constraints data will be added using attributes
 %%                 </description>
 %%         </ElementType>
 %%         <ElementType name="price" content="textOnly">
 %%                 <description>Price data will be added using attributes
 %%                 </description>
 %%         </ElementType>
