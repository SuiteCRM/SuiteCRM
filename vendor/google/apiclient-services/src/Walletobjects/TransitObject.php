<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Walletobjects;

class TransitObject extends \Google\Collection
{
  protected $collection_key = 'valueAddedModuleData';
  protected $activationStatusType = ActivationStatus::class;
  protected $activationStatusDataType = '';
  protected $appLinkDataType = AppLinkData::class;
  protected $appLinkDataDataType = '';
  protected $barcodeType = Barcode::class;
  protected $barcodeDataType = '';
  /**
   * @var string
   */
  public $classId;
  protected $classReferenceType = TransitClass::class;
  protected $classReferenceDataType = '';
  /**
   * @var string
   */
  public $concessionCategory;
  protected $customConcessionCategoryType = LocalizedString::class;
  protected $customConcessionCategoryDataType = '';
  protected $customTicketStatusType = LocalizedString::class;
  protected $customTicketStatusDataType = '';
  protected $deviceContextType = DeviceContext::class;
  protected $deviceContextDataType = '';
  /**
   * @var bool
   */
  public $disableExpirationNotification;
  protected $groupingInfoType = GroupingInfo::class;
  protected $groupingInfoDataType = '';
  /**
   * @var bool
   */
  public $hasLinkedDevice;
  /**
   * @var bool
   */
  public $hasUsers;
  protected $heroImageType = Image::class;
  protected $heroImageDataType = '';
  /**
   * @var string
   */
  public $hexBackgroundColor;
  /**
   * @var string
   */
  public $id;
  protected $imageModulesDataType = ImageModuleData::class;
  protected $imageModulesDataDataType = 'array';
  protected $infoModuleDataType = InfoModuleData::class;
  protected $infoModuleDataDataType = '';
  /**
   * @var string[]
   */
  public $linkedObjectIds;
  protected $linksModuleDataType = LinksModuleData::class;
  protected $linksModuleDataDataType = '';
  protected $locationsType = LatLongPoint::class;
  protected $locationsDataType = 'array';
  protected $merchantLocationsType = MerchantLocation::class;
  protected $merchantLocationsDataType = 'array';
  protected $messagesType = Message::class;
  protected $messagesDataType = 'array';
  /**
   * @var string
   */
  public $notifyPreference;
  protected $passConstraintsType = PassConstraints::class;
  protected $passConstraintsDataType = '';
  /**
   * @var string
   */
  public $passengerNames;
  /**
   * @var string
   */
  public $passengerType;
  protected $purchaseDetailsType = PurchaseDetails::class;
  protected $purchaseDetailsDataType = '';
  protected $rotatingBarcodeType = RotatingBarcode::class;
  protected $rotatingBarcodeDataType = '';
  protected $saveRestrictionsType = SaveRestrictions::class;
  protected $saveRestrictionsDataType = '';
  /**
   * @var string
   */
  public $smartTapRedemptionValue;
  /**
   * @var string
   */
  public $state;
  protected $textModulesDataType = TextModuleData::class;
  protected $textModulesDataDataType = 'array';
  protected $ticketLegType = TicketLeg::class;
  protected $ticketLegDataType = '';
  protected $ticketLegsType = TicketLeg::class;
  protected $ticketLegsDataType = 'array';
  /**
   * @var string
   */
  public $ticketNumber;
  protected $ticketRestrictionsType = TicketRestrictions::class;
  protected $ticketRestrictionsDataType = '';
  /**
   * @var string
   */
  public $ticketStatus;
  /**
   * @var string
   */
  public $tripId;
  /**
   * @var string
   */
  public $tripType;
  protected $validTimeIntervalType = TimeInterval::class;
  protected $validTimeIntervalDataType = '';
  protected $valueAddedModuleDataType = ValueAddedModuleData::class;
  protected $valueAddedModuleDataDataType = 'array';
  /**
   * @var string
   */
  public $version;

  /**
   * @param ActivationStatus
   */
  public function setActivationStatus(ActivationStatus $activationStatus)
  {
    $this->activationStatus = $activationStatus;
  }
  /**
   * @return ActivationStatus
   */
  public function getActivationStatus()
  {
    return $this->activationStatus;
  }
  /**
   * @param AppLinkData
   */
  public function setAppLinkData(AppLinkData $appLinkData)
  {
    $this->appLinkData = $appLinkData;
  }
  /**
   * @return AppLinkData
   */
  public function getAppLinkData()
  {
    return $this->appLinkData;
  }
  /**
   * @param Barcode
   */
  public function setBarcode(Barcode $barcode)
  {
    $this->barcode = $barcode;
  }
  /**
   * @return Barcode
   */
  public function getBarcode()
  {
    return $this->barcode;
  }
  /**
   * @param string
   */
  public function setClassId($classId)
  {
    $this->classId = $classId;
  }
  /**
   * @return string
   */
  public function getClassId()
  {
    return $this->classId;
  }
  /**
   * @param TransitClass
   */
  public function setClassReference(TransitClass $classReference)
  {
    $this->classReference = $classReference;
  }
  /**
   * @return TransitClass
   */
  public function getClassReference()
  {
    return $this->classReference;
  }
  /**
   * @param string
   */
  public function setConcessionCategory($concessionCategory)
  {
    $this->concessionCategory = $concessionCategory;
  }
  /**
   * @return string
   */
  public function getConcessionCategory()
  {
    return $this->concessionCategory;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomConcessionCategory(LocalizedString $customConcessionCategory)
  {
    $this->customConcessionCategory = $customConcessionCategory;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomConcessionCategory()
  {
    return $this->customConcessionCategory;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomTicketStatus(LocalizedString $customTicketStatus)
  {
    $this->customTicketStatus = $customTicketStatus;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomTicketStatus()
  {
    return $this->customTicketStatus;
  }
  /**
   * @param DeviceContext
   */
  public function setDeviceContext(DeviceContext $deviceContext)
  {
    $this->deviceContext = $deviceContext;
  }
  /**
   * @return DeviceContext
   */
  public function getDeviceContext()
  {
    return $this->deviceContext;
  }
  /**
   * @param bool
   */
  public function setDisableExpirationNotification($disableExpirationNotification)
  {
    $this->disableExpirationNotification = $disableExpirationNotification;
  }
  /**
   * @return bool
   */
  public function getDisableExpirationNotification()
  {
    return $this->disableExpirationNotification;
  }
  /**
   * @param GroupingInfo
   */
  public function setGroupingInfo(GroupingInfo $groupingInfo)
  {
    $this->groupingInfo = $groupingInfo;
  }
  /**
   * @return GroupingInfo
   */
  public function getGroupingInfo()
  {
    return $this->groupingInfo;
  }
  /**
   * @param bool
   */
  public function setHasLinkedDevice($hasLinkedDevice)
  {
    $this->hasLinkedDevice = $hasLinkedDevice;
  }
  /**
   * @return bool
   */
  public function getHasLinkedDevice()
  {
    return $this->hasLinkedDevice;
  }
  /**
   * @param bool
   */
  public function setHasUsers($hasUsers)
  {
    $this->hasUsers = $hasUsers;
  }
  /**
   * @return bool
   */
  public function getHasUsers()
  {
    return $this->hasUsers;
  }
  /**
   * @param Image
   */
  public function setHeroImage(Image $heroImage)
  {
    $this->heroImage = $heroImage;
  }
  /**
   * @return Image
   */
  public function getHeroImage()
  {
    return $this->heroImage;
  }
  /**
   * @param string
   */
  public function setHexBackgroundColor($hexBackgroundColor)
  {
    $this->hexBackgroundColor = $hexBackgroundColor;
  }
  /**
   * @return string
   */
  public function getHexBackgroundColor()
  {
    return $this->hexBackgroundColor;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param ImageModuleData[]
   */
  public function setImageModulesData($imageModulesData)
  {
    $this->imageModulesData = $imageModulesData;
  }
  /**
   * @return ImageModuleData[]
   */
  public function getImageModulesData()
  {
    return $this->imageModulesData;
  }
  /**
   * @param InfoModuleData
   */
  public function setInfoModuleData(InfoModuleData $infoModuleData)
  {
    $this->infoModuleData = $infoModuleData;
  }
  /**
   * @return InfoModuleData
   */
  public function getInfoModuleData()
  {
    return $this->infoModuleData;
  }
  /**
   * @param string[]
   */
  public function setLinkedObjectIds($linkedObjectIds)
  {
    $this->linkedObjectIds = $linkedObjectIds;
  }
  /**
   * @return string[]
   */
  public function getLinkedObjectIds()
  {
    return $this->linkedObjectIds;
  }
  /**
   * @param LinksModuleData
   */
  public function setLinksModuleData(LinksModuleData $linksModuleData)
  {
    $this->linksModuleData = $linksModuleData;
  }
  /**
   * @return LinksModuleData
   */
  public function getLinksModuleData()
  {
    return $this->linksModuleData;
  }
  /**
   * @param LatLongPoint[]
   */
  public function setLocations($locations)
  {
    $this->locations = $locations;
  }
  /**
   * @return LatLongPoint[]
   */
  public function getLocations()
  {
    return $this->locations;
  }
  /**
   * @param MerchantLocation[]
   */
  public function setMerchantLocations($merchantLocations)
  {
    $this->merchantLocations = $merchantLocations;
  }
  /**
   * @return MerchantLocation[]
   */
  public function getMerchantLocations()
  {
    return $this->merchantLocations;
  }
  /**
   * @param Message[]
   */
  public function setMessages($messages)
  {
    $this->messages = $messages;
  }
  /**
   * @return Message[]
   */
  public function getMessages()
  {
    return $this->messages;
  }
  /**
   * @param string
   */
  public function setNotifyPreference($notifyPreference)
  {
    $this->notifyPreference = $notifyPreference;
  }
  /**
   * @return string
   */
  public function getNotifyPreference()
  {
    return $this->notifyPreference;
  }
  /**
   * @param PassConstraints
   */
  public function setPassConstraints(PassConstraints $passConstraints)
  {
    $this->passConstraints = $passConstraints;
  }
  /**
   * @return PassConstraints
   */
  public function getPassConstraints()
  {
    return $this->passConstraints;
  }
  /**
   * @param string
   */
  public function setPassengerNames($passengerNames)
  {
    $this->passengerNames = $passengerNames;
  }
  /**
   * @return string
   */
  public function getPassengerNames()
  {
    return $this->passengerNames;
  }
  /**
   * @param string
   */
  public function setPassengerType($passengerType)
  {
    $this->passengerType = $passengerType;
  }
  /**
   * @return string
   */
  public function getPassengerType()
  {
    return $this->passengerType;
  }
  /**
   * @param PurchaseDetails
   */
  public function setPurchaseDetails(PurchaseDetails $purchaseDetails)
  {
    $this->purchaseDetails = $purchaseDetails;
  }
  /**
   * @return PurchaseDetails
   */
  public function getPurchaseDetails()
  {
    return $this->purchaseDetails;
  }
  /**
   * @param RotatingBarcode
   */
  public function setRotatingBarcode(RotatingBarcode $rotatingBarcode)
  {
    $this->rotatingBarcode = $rotatingBarcode;
  }
  /**
   * @return RotatingBarcode
   */
  public function getRotatingBarcode()
  {
    return $this->rotatingBarcode;
  }
  /**
   * @param SaveRestrictions
   */
  public function setSaveRestrictions(SaveRestrictions $saveRestrictions)
  {
    $this->saveRestrictions = $saveRestrictions;
  }
  /**
   * @return SaveRestrictions
   */
  public function getSaveRestrictions()
  {
    return $this->saveRestrictions;
  }
  /**
   * @param string
   */
  public function setSmartTapRedemptionValue($smartTapRedemptionValue)
  {
    $this->smartTapRedemptionValue = $smartTapRedemptionValue;
  }
  /**
   * @return string
   */
  public function getSmartTapRedemptionValue()
  {
    return $this->smartTapRedemptionValue;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param TextModuleData[]
   */
  public function setTextModulesData($textModulesData)
  {
    $this->textModulesData = $textModulesData;
  }
  /**
   * @return TextModuleData[]
   */
  public function getTextModulesData()
  {
    return $this->textModulesData;
  }
  /**
   * @param TicketLeg
   */
  public function setTicketLeg(TicketLeg $ticketLeg)
  {
    $this->ticketLeg = $ticketLeg;
  }
  /**
   * @return TicketLeg
   */
  public function getTicketLeg()
  {
    return $this->ticketLeg;
  }
  /**
   * @param TicketLeg[]
   */
  public function setTicketLegs($ticketLegs)
  {
    $this->ticketLegs = $ticketLegs;
  }
  /**
   * @return TicketLeg[]
   */
  public function getTicketLegs()
  {
    return $this->ticketLegs;
  }
  /**
   * @param string
   */
  public function setTicketNumber($ticketNumber)
  {
    $this->ticketNumber = $ticketNumber;
  }
  /**
   * @return string
   */
  public function getTicketNumber()
  {
    return $this->ticketNumber;
  }
  /**
   * @param TicketRestrictions
   */
  public function setTicketRestrictions(TicketRestrictions $ticketRestrictions)
  {
    $this->ticketRestrictions = $ticketRestrictions;
  }
  /**
   * @return TicketRestrictions
   */
  public function getTicketRestrictions()
  {
    return $this->ticketRestrictions;
  }
  /**
   * @param string
   */
  public function setTicketStatus($ticketStatus)
  {
    $this->ticketStatus = $ticketStatus;
  }
  /**
   * @return string
   */
  public function getTicketStatus()
  {
    return $this->ticketStatus;
  }
  /**
   * @param string
   */
  public function setTripId($tripId)
  {
    $this->tripId = $tripId;
  }
  /**
   * @return string
   */
  public function getTripId()
  {
    return $this->tripId;
  }
  /**
   * @param string
   */
  public function setTripType($tripType)
  {
    $this->tripType = $tripType;
  }
  /**
   * @return string
   */
  public function getTripType()
  {
    return $this->tripType;
  }
  /**
   * @param TimeInterval
   */
  public function setValidTimeInterval(TimeInterval $validTimeInterval)
  {
    $this->validTimeInterval = $validTimeInterval;
  }
  /**
   * @return TimeInterval
   */
  public function getValidTimeInterval()
  {
    return $this->validTimeInterval;
  }
  /**
   * @param ValueAddedModuleData[]
   */
  public function setValueAddedModuleData($valueAddedModuleData)
  {
    $this->valueAddedModuleData = $valueAddedModuleData;
  }
  /**
   * @return ValueAddedModuleData[]
   */
  public function getValueAddedModuleData()
  {
    return $this->valueAddedModuleData;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TransitObject::class, 'Google_Service_Walletobjects_TransitObject');
