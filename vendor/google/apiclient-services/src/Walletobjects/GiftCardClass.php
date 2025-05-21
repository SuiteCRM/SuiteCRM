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

class GiftCardClass extends \Google\Collection
{
  protected $collection_key = 'valueAddedModuleData';
  /**
   * @var bool
   */
  public $allowBarcodeRedemption;
  /**
   * @var bool
   */
  public $allowMultipleUsersPerObject;
  protected $appLinkDataType = AppLinkData::class;
  protected $appLinkDataDataType = '';
  protected $callbackOptionsType = CallbackOptions::class;
  protected $callbackOptionsDataType = '';
  /**
   * @var string
   */
  public $cardNumberLabel;
  protected $classTemplateInfoType = ClassTemplateInfo::class;
  protected $classTemplateInfoDataType = '';
  /**
   * @var string
   */
  public $countryCode;
  /**
   * @var bool
   */
  public $enableSmartTap;
  /**
   * @var string
   */
  public $eventNumberLabel;
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
  protected $localizedCardNumberLabelType = LocalizedString::class;
  protected $localizedCardNumberLabelDataType = '';
  protected $localizedEventNumberLabelType = LocalizedString::class;
  protected $localizedEventNumberLabelDataType = '';
  protected $localizedIssuerNameType = LocalizedString::class;
  protected $localizedIssuerNameDataType = '';
  protected $localizedMerchantNameType = LocalizedString::class;
  protected $localizedMerchantNameDataType = '';
  protected $localizedPinLabelType = LocalizedString::class;
  protected $localizedPinLabelDataType = '';
  protected $locationsType = LatLongPoint::class;
  protected $locationsDataType = 'array';
  protected $merchantLocationsType = MerchantLocation::class;
  protected $merchantLocationsDataType = 'array';
  /**
   * @var string
   */
  public $merchantName;
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
   * @var string
   */
  public $pinLabel;
  protected $programLogoType = Image::class;
  protected $programLogoDataType = '';
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
  protected $securityAnimationType = SecurityAnimation::class;
  protected $securityAnimationDataType = '';
  protected $textModulesDataType = TextModuleData::class;
  protected $textModulesDataDataType = 'array';
  protected $valueAddedModuleDataType = ValueAddedModuleData::class;
  protected $valueAddedModuleDataDataType = 'array';
  /**
   * @var string
   */
  public $version;
  /**
   * @var string
   */
  public $viewUnlockRequirement;
  protected $wideProgramLogoType = Image::class;
  protected $wideProgramLogoDataType = '';
  protected $wordMarkType = Image::class;
  protected $wordMarkDataType = '';

  /**
   * @param bool
   */
  public function setAllowBarcodeRedemption($allowBarcodeRedemption)
  {
    $this->allowBarcodeRedemption = $allowBarcodeRedemption;
  }
  /**
   * @return bool
   */
  public function getAllowBarcodeRedemption()
  {
    return $this->allowBarcodeRedemption;
  }
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
   * @param string
   */
  public function setCardNumberLabel($cardNumberLabel)
  {
    $this->cardNumberLabel = $cardNumberLabel;
  }
  /**
   * @return string
   */
  public function getCardNumberLabel()
  {
    return $this->cardNumberLabel;
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
  public function setEventNumberLabel($eventNumberLabel)
  {
    $this->eventNumberLabel = $eventNumberLabel;
  }
  /**
   * @return string
   */
  public function getEventNumberLabel()
  {
    return $this->eventNumberLabel;
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
  public function setLocalizedCardNumberLabel(LocalizedString $localizedCardNumberLabel)
  {
    $this->localizedCardNumberLabel = $localizedCardNumberLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedCardNumberLabel()
  {
    return $this->localizedCardNumberLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedEventNumberLabel(LocalizedString $localizedEventNumberLabel)
  {
    $this->localizedEventNumberLabel = $localizedEventNumberLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedEventNumberLabel()
  {
    return $this->localizedEventNumberLabel;
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
   * @param LocalizedString
   */
  public function setLocalizedMerchantName(LocalizedString $localizedMerchantName)
  {
    $this->localizedMerchantName = $localizedMerchantName;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedMerchantName()
  {
    return $this->localizedMerchantName;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedPinLabel(LocalizedString $localizedPinLabel)
  {
    $this->localizedPinLabel = $localizedPinLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedPinLabel()
  {
    return $this->localizedPinLabel;
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
   * @param string
   */
  public function setMerchantName($merchantName)
  {
    $this->merchantName = $merchantName;
  }
  /**
   * @return string
   */
  public function getMerchantName()
  {
    return $this->merchantName;
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
   * @param string
   */
  public function setPinLabel($pinLabel)
  {
    $this->pinLabel = $pinLabel;
  }
  /**
   * @return string
   */
  public function getPinLabel()
  {
    return $this->pinLabel;
  }
  /**
   * @param Image
   */
  public function setProgramLogo(Image $programLogo)
  {
    $this->programLogo = $programLogo;
  }
  /**
   * @return Image
   */
  public function getProgramLogo()
  {
    return $this->programLogo;
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
  public function setWideProgramLogo(Image $wideProgramLogo)
  {
    $this->wideProgramLogo = $wideProgramLogo;
  }
  /**
   * @return Image
   */
  public function getWideProgramLogo()
  {
    return $this->wideProgramLogo;
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
class_alias(GiftCardClass::class, 'Google_Service_Walletobjects_GiftCardClass');
