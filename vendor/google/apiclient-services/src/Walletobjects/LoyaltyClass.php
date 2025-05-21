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

class LoyaltyClass extends \Google\Collection
{
  protected $collection_key = 'valueAddedModuleData';
  /**
   * @var string
   */
  public $accountIdLabel;
  /**
   * @var string
   */
  public $accountNameLabel;
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
  public $countryCode;
  protected $discoverableProgramType = DiscoverableProgram::class;
  protected $discoverableProgramDataType = '';
  /**
   * @var bool
   */
  public $enableSmartTap;
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
  protected $localizedAccountIdLabelType = LocalizedString::class;
  protected $localizedAccountIdLabelDataType = '';
  protected $localizedAccountNameLabelType = LocalizedString::class;
  protected $localizedAccountNameLabelDataType = '';
  protected $localizedIssuerNameType = LocalizedString::class;
  protected $localizedIssuerNameDataType = '';
  protected $localizedProgramNameType = LocalizedString::class;
  protected $localizedProgramNameDataType = '';
  protected $localizedRewardsTierType = LocalizedString::class;
  protected $localizedRewardsTierDataType = '';
  protected $localizedRewardsTierLabelType = LocalizedString::class;
  protected $localizedRewardsTierLabelDataType = '';
  protected $localizedSecondaryRewardsTierType = LocalizedString::class;
  protected $localizedSecondaryRewardsTierDataType = '';
  protected $localizedSecondaryRewardsTierLabelType = LocalizedString::class;
  protected $localizedSecondaryRewardsTierLabelDataType = '';
  protected $locationsType = LatLongPoint::class;
  protected $locationsDataType = 'array';
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
  protected $programLogoType = Image::class;
  protected $programLogoDataType = '';
  /**
   * @var string
   */
  public $programName;
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
  public $rewardsTier;
  /**
   * @var string
   */
  public $rewardsTierLabel;
  /**
   * @var string
   */
  public $secondaryRewardsTier;
  /**
   * @var string
   */
  public $secondaryRewardsTierLabel;
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
   * @param string
   */
  public function setAccountIdLabel($accountIdLabel)
  {
    $this->accountIdLabel = $accountIdLabel;
  }
  /**
   * @return string
   */
  public function getAccountIdLabel()
  {
    return $this->accountIdLabel;
  }
  /**
   * @param string
   */
  public function setAccountNameLabel($accountNameLabel)
  {
    $this->accountNameLabel = $accountNameLabel;
  }
  /**
   * @return string
   */
  public function getAccountNameLabel()
  {
    return $this->accountNameLabel;
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
   * @param DiscoverableProgram
   */
  public function setDiscoverableProgram(DiscoverableProgram $discoverableProgram)
  {
    $this->discoverableProgram = $discoverableProgram;
  }
  /**
   * @return DiscoverableProgram
   */
  public function getDiscoverableProgram()
  {
    return $this->discoverableProgram;
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
  public function setLocalizedAccountIdLabel(LocalizedString $localizedAccountIdLabel)
  {
    $this->localizedAccountIdLabel = $localizedAccountIdLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedAccountIdLabel()
  {
    return $this->localizedAccountIdLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedAccountNameLabel(LocalizedString $localizedAccountNameLabel)
  {
    $this->localizedAccountNameLabel = $localizedAccountNameLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedAccountNameLabel()
  {
    return $this->localizedAccountNameLabel;
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
  public function setLocalizedProgramName(LocalizedString $localizedProgramName)
  {
    $this->localizedProgramName = $localizedProgramName;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedProgramName()
  {
    return $this->localizedProgramName;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedRewardsTier(LocalizedString $localizedRewardsTier)
  {
    $this->localizedRewardsTier = $localizedRewardsTier;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedRewardsTier()
  {
    return $this->localizedRewardsTier;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedRewardsTierLabel(LocalizedString $localizedRewardsTierLabel)
  {
    $this->localizedRewardsTierLabel = $localizedRewardsTierLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedRewardsTierLabel()
  {
    return $this->localizedRewardsTierLabel;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedSecondaryRewardsTier(LocalizedString $localizedSecondaryRewardsTier)
  {
    $this->localizedSecondaryRewardsTier = $localizedSecondaryRewardsTier;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedSecondaryRewardsTier()
  {
    return $this->localizedSecondaryRewardsTier;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedSecondaryRewardsTierLabel(LocalizedString $localizedSecondaryRewardsTierLabel)
  {
    $this->localizedSecondaryRewardsTierLabel = $localizedSecondaryRewardsTierLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedSecondaryRewardsTierLabel()
  {
    return $this->localizedSecondaryRewardsTierLabel;
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
   * @param string
   */
  public function setProgramName($programName)
  {
    $this->programName = $programName;
  }
  /**
   * @return string
   */
  public function getProgramName()
  {
    return $this->programName;
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
  public function setRewardsTier($rewardsTier)
  {
    $this->rewardsTier = $rewardsTier;
  }
  /**
   * @return string
   */
  public function getRewardsTier()
  {
    return $this->rewardsTier;
  }
  /**
   * @param string
   */
  public function setRewardsTierLabel($rewardsTierLabel)
  {
    $this->rewardsTierLabel = $rewardsTierLabel;
  }
  /**
   * @return string
   */
  public function getRewardsTierLabel()
  {
    return $this->rewardsTierLabel;
  }
  /**
   * @param string
   */
  public function setSecondaryRewardsTier($secondaryRewardsTier)
  {
    $this->secondaryRewardsTier = $secondaryRewardsTier;
  }
  /**
   * @return string
   */
  public function getSecondaryRewardsTier()
  {
    return $this->secondaryRewardsTier;
  }
  /**
   * @param string
   */
  public function setSecondaryRewardsTierLabel($secondaryRewardsTierLabel)
  {
    $this->secondaryRewardsTierLabel = $secondaryRewardsTierLabel;
  }
  /**
   * @return string
   */
  public function getSecondaryRewardsTierLabel()
  {
    return $this->secondaryRewardsTierLabel;
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
class_alias(LoyaltyClass::class, 'Google_Service_Walletobjects_LoyaltyClass');
