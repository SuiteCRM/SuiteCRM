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

class EventTicketClass extends \Google\Collection
{
  protected $collection_key = 'valueAddedModuleData';
  /**
   * @var bool
   */
  public $allowMultipleUsersPerObject;
  protected $appLinkDataType = AppLinkData::class;
  protected $appLinkDataDataType = '';
  protected $callbackOptionsType = CallbackOptions::class;
  protected $callbackOptionsDataType = '';
  protected $classTemplateInfoType = ClassTemplateInfo::class;
  protected $classTemplateInfoDataType = '';
  /**
   * @var string
   */
  public $confirmationCodeLabel;
  /**
   * @var string
   */
  public $countryCode;
  protected $customConfirmationCodeLabelType = LocalizedString::class;
  protected $customConfirmationCodeLabelDataType = '';
  protected $customGateLabelType = LocalizedString::class;
  protected $customGateLabelDataType = '';
  protected $customRowLabelType = LocalizedString::class;
  protected $customRowLabelDataType = '';
  protected $customSeatLabelType = LocalizedString::class;
  protected $customSeatLabelDataType = '';
  protected $customSectionLabelType = LocalizedString::class;
  protected $customSectionLabelDataType = '';
  protected $dateTimeType = EventDateTime::class;
  protected $dateTimeDataType = '';
  /**
   * @var bool
   */
  public $enableSmartTap;
  /**
   * @var string
   */
  public $eventId;
  protected $eventNameType = LocalizedString::class;
  protected $eventNameDataType = '';
  protected $finePrintType = LocalizedString::class;
  protected $finePrintDataType = '';
  /**
   * @var string
   */
  public $gateLabel;
  protected $heroImageType = Image::class;
  protected $heroImageDataType = '';
  /**
   * @var string
   */
  public $hexBackgroundColor;
  protected $homepageUriType = Uri::class;
  protected $homepageUriDataType = '';
  /**
   * @var string
   */
  public $id;
  protected $imageModulesDataType = ImageModuleData::class;
  protected $imageModulesDataDataType = 'array';
  protected $infoModuleDataType = InfoModuleData::class;
  protected $infoModuleDataDataType = '';
  /**
   * @var string
   */
  public $issuerName;
  /**
   * @var string
   */
  public $kind;
  protected $linksModuleDataType = LinksModuleData::class;
  protected $linksModuleDataDataType = '';
  protected $localizedIssuerNameType = LocalizedString::class;
  protected $localizedIssuerNameDataType = '';
  protected $locationsType = LatLongPoint::class;
  protected $locationsDataType = 'array';
  protected $logoType = Image::class;
  protected $logoDataType = '';
  protected $merchantLocationsType = MerchantLocation::class;
  protected $merchantLocationsDataType = 'array';
  protected $messagesType = Message::class;
  protected $messagesDataType = 'array';
  /**
   * @var string
   */
  public $multipleDevicesAndHoldersAllowedStatus;
  /**
   * @var string
   */
  public $notifyPreference;
  /**
   * @var string[]
   */
  public $redemptionIssuers;
  protected $reviewType = Review::class;
  protected $reviewDataType = '';
  /**
   * @var string
   */
  public $reviewStatus;
  /**
   * @var string
   */
  public $rowLabel;
  /**
   * @var string
   */
  public $seatLabel;
  /**
   * @var string
   */
  public $sectionLabel;
  protected $securityAnimationType = SecurityAnimation::class;
  protected $securityAnimationDataType = '';
  protected $textModulesDataType = TextModuleData::class;
  protected $textModulesDataDataType = 'array';
  protected $valueAddedModuleDataType = ValueAddedModuleData::class;
  protected $valueAddedModuleDataDataType = 'array';
  protected $venueType = EventVenue::class;
  protected $venueDataType = '';
  /**
   * @var string
   */
  public $version;
  /**
   * @var string
   */
  public $viewUnlockRequirement;
  protected $wideLogoType = Image::class;
  protected $wideLogoDataType = '';
  protected $wordMarkType = Image::class;
  protected $wordMarkDataType = '';

  /**
   * @param bool
   */
  public function setAllowMultipleUsersPerObject($allowMultipleUsersPerObject)
  {
    $this->allowMultipleUsersPerObject = $allowMultipleUsersPerObject;
  }
  /**
   * @return bool
   */
  public function getAllowMultipleUsersPerObject()
  {
    return $this->allowMultipleUsersPerObject;
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
   * @param CallbackOptions
   */
  public function setCallbackOptions(CallbackOptions $callbackOptions)
  {
    $this->callbackOptions = $callbackOptions;
  }
  /**
   * @return CallbackOptions
   */
  public function getCallbackOptions()
  {
    return $this->callbackOptions;
  }
  /**
   * @param ClassTemplateInfo
   */
  public function setClassTemplateInfo(ClassTemplateInfo $classTemplateInfo)
  {
    $this->classTemplateInfo = $classTemplateInfo;
  }
  /**
   * @return ClassTemplateInfo
   */
  public function getClassTemplateInfo()
  {
    return $this->classTemplateInfo;
  }
  /**
   * @param string
   */
  public function setConfirmationCodeLabel($confirmationCodeLabel)
  {
    $this->confirmationCodeLabel = $confirmationCodeLabel;
  }
  /**
   * @return string
   */
  public function getConfirmationCodeLabel()
  {
    return $this->confirmationCodeLabel;
  }
  /**
   * @param string
   */
  public function setCountryCode($countryCode)
  {
    $this->countryCode = $countryCode;
  }
  /**
   * @return string
   */
  public function getCountryCode()
  {
    return $this->countryCode;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomConfirmationCodeLabel(LocalizedString $customConfirmationCodeLabel)
  {
    $this->customConfirmationCodeLabel = $customConfirmationCodeLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomConfirmationCodeLabel()
  {
    return $this->customConfirmationCodeLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomGateLabel(LocalizedString $customGateLabel)
  {
    $this->customGateLabel = $customGateLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomGateLabel()
  {
    return $this->customGateLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomRowLabel(LocalizedString $customRowLabel)
  {
    $this->customRowLabel = $customRowLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomRowLabel()
  {
    return $this->customRowLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomSeatLabel(LocalizedString $customSeatLabel)
  {
    $this->customSeatLabel = $customSeatLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomSeatLabel()
  {
    return $this->customSeatLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomSectionLabel(LocalizedString $customSectionLabel)
  {
    $this->customSectionLabel = $customSectionLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomSectionLabel()
  {
    return $this->customSectionLabel;
  }
  /**
   * @param EventDateTime
   */
  public function setDateTime(EventDateTime $dateTime)
  {
    $this->dateTime = $dateTime;
  }
  /**
   * @return EventDateTime
   */
  public function getDateTime()
  {
    return $this->dateTime;
  }
  /**
   * @param bool
   */
  public function setEnableSmartTap($enableSmartTap)
  {
    $this->enableSmartTap = $enableSmartTap;
  }
  /**
   * @return bool
   */
  public function getEnableSmartTap()
  {
    return $this->enableSmartTap;
  }
  /**
   * @param string
   */
  public function setEventId($eventId)
  {
    $this->eventId = $eventId;
  }
  /**
   * @return string
   */
  public function getEventId()
  {
    return $this->eventId;
  }
  /**
   * @param LocalizedString
   */
  public function setEventName(LocalizedString $eventName)
  {
    $this->eventName = $eventName;
  }
  /**
   * @return LocalizedString
   */
  public function getEventName()
  {
    return $this->eventName;
  }
  /**
   * @param LocalizedString
   */
  public function setFinePrint(LocalizedString $finePrint)
  {
    $this->finePrint = $finePrint;
  }
  /**
   * @return LocalizedString
   */
  public function getFinePrint()
  {
    return $this->finePrint;
  }
  /**
   * @param string
   */
  public function setGateLabel($gateLabel)
  {
    $this->gateLabel = $gateLabel;
  }
  /**
   * @return string
   */
  public function getGateLabel()
  {
    return $this->gateLabel;
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
   * @param Uri
   */
  public function setHomepageUri(Uri $homepageUri)
  {
    $this->homepageUri = $homepageUri;
  }
  /**
   * @return Uri
   */
  public function getHomepageUri()
  {
    return $this->homepageUri;
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
   * @param string
   */
  public function setIssuerName($issuerName)
  {
    $this->issuerName = $issuerName;
  }
  /**
   * @return string
   */
  public function getIssuerName()
  {
    return $this->issuerName;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
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
   * @param LocalizedString
   */
  public function setLocalizedIssuerName(LocalizedString $localizedIssuerName)
  {
    $this->localizedIssuerName = $localizedIssuerName;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedIssuerName()
  {
    return $this->localizedIssuerName;
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
   * @param Image
   */
  public function setLogo(Image $logo)
  {
    $this->logo = $logo;
  }
  /**
   * @return Image
   */
  public function getLogo()
  {
    return $this->logo;
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
  public function setMultipleDevicesAndHoldersAllowedStatus($multipleDevicesAndHoldersAllowedStatus)
  {
    $this->multipleDevicesAndHoldersAllowedStatus = $multipleDevicesAndHoldersAllowedStatus;
  }
  /**
   * @return string
   */
  public function getMultipleDevicesAndHoldersAllowedStatus()
  {
    return $this->multipleDevicesAndHoldersAllowedStatus;
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
   * @param string[]
   */
  public function setRedemptionIssuers($redemptionIssuers)
  {
    $this->redemptionIssuers = $redemptionIssuers;
  }
  /**
   * @return string[]
   */
  public function getRedemptionIssuers()
  {
    return $this->redemptionIssuers;
  }
  /**
   * @param Review
   */
  public function setReview(Review $review)
  {
    $this->review = $review;
  }
  /**
   * @return Review
   */
  public function getReview()
  {
    return $this->review;
  }
  /**
   * @param string
   */
  public function setReviewStatus($reviewStatus)
  {
    $this->reviewStatus = $reviewStatus;
  }
  /**
   * @return string
   */
  public function getReviewStatus()
  {
    return $this->reviewStatus;
  }
  /**
   * @param string
   */
  public function setRowLabel($rowLabel)
  {
    $this->rowLabel = $rowLabel;
  }
  /**
   * @return string
   */
  public function getRowLabel()
  {
    return $this->rowLabel;
  }
  /**
   * @param string
   */
  public function setSeatLabel($seatLabel)
  {
    $this->seatLabel = $seatLabel;
  }
  /**
   * @return string
   */
  public function getSeatLabel()
  {
    return $this->seatLabel;
  }
  /**
   * @param string
   */
  public function setSectionLabel($sectionLabel)
  {
    $this->sectionLabel = $sectionLabel;
  }
  /**
   * @return string
   */
  public function getSectionLabel()
  {
    return $this->sectionLabel;
  }
  /**
   * @param SecurityAnimation
   */
  public function setSecurityAnimation(SecurityAnimation $securityAnimation)
  {
    $this->securityAnimation = $securityAnimation;
  }
  /**
   * @return SecurityAnimation
   */
  public function getSecurityAnimation()
  {
    return $this->securityAnimation;
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
   * @param EventVenue
   */
  public function setVenue(EventVenue $venue)
  {
    $this->venue = $venue;
  }
  /**
   * @return EventVenue
   */
  public function getVenue()
  {
    return $this->venue;
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
  /**
   * @param string
   */
  public function setViewUnlockRequirement($viewUnlockRequirement)
  {
    $this->viewUnlockRequirement = $viewUnlockRequirement;
  }
  /**
   * @return string
   */
  public function getViewUnlockRequirement()
  {
    return $this->viewUnlockRequirement;
  }
  /**
   * @param Image
   */
  public function setWideLogo(Image $wideLogo)
  {
    $this->wideLogo = $wideLogo;
  }
  /**
   * @return Image
   */
  public function getWideLogo()
  {
    return $this->wideLogo;
  }
  /**
   * @param Image
   */
  public function setWordMark(Image $wordMark)
  {
    $this->wordMark = $wordMark;
  }
  /**
   * @return Image
   */
  public function getWordMark()
  {
    return $this->wordMark;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EventTicketClass::class, 'Google_Service_Walletobjects_EventTicketClass');
