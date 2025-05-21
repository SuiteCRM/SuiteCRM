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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesCampaignCriterion extends \Google\Model
{
  protected $ageRangeType = GoogleAdsSearchads360V0CommonAgeRangeInfo::class;
  protected $ageRangeDataType = '';
  /**
   * @var float
   */
  public $bidModifier;
  /**
   * @var string
   */
  public $criterionId;
  protected $deviceType = GoogleAdsSearchads360V0CommonDeviceInfo::class;
  protected $deviceDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $genderType = GoogleAdsSearchads360V0CommonGenderInfo::class;
  protected $genderDataType = '';
  protected $keywordType = GoogleAdsSearchads360V0CommonKeywordInfo::class;
  protected $keywordDataType = '';
  protected $languageType = GoogleAdsSearchads360V0CommonLanguageInfo::class;
  protected $languageDataType = '';
  /**
   * @var string
   */
  public $lastModifiedTime;
  protected $locationType = GoogleAdsSearchads360V0CommonLocationInfo::class;
  protected $locationDataType = '';
  protected $locationGroupType = GoogleAdsSearchads360V0CommonLocationGroupInfo::class;
  protected $locationGroupDataType = '';
  /**
   * @var bool
   */
  public $negative;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $type;
  protected $userListType = GoogleAdsSearchads360V0CommonUserListInfo::class;
  protected $userListDataType = '';
  protected $webpageType = GoogleAdsSearchads360V0CommonWebpageInfo::class;
  protected $webpageDataType = '';

  /**
   * @param GoogleAdsSearchads360V0CommonAgeRangeInfo
   */
  public function setAgeRange(GoogleAdsSearchads360V0CommonAgeRangeInfo $ageRange)
  {
    $this->ageRange = $ageRange;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonAgeRangeInfo
   */
  public function getAgeRange()
  {
    return $this->ageRange;
  }
  /**
   * @param float
   */
  public function setBidModifier($bidModifier)
  {
    $this->bidModifier = $bidModifier;
  }
  /**
   * @return float
   */
  public function getBidModifier()
  {
    return $this->bidModifier;
  }
  /**
   * @param string
   */
  public function setCriterionId($criterionId)
  {
    $this->criterionId = $criterionId;
  }
  /**
   * @return string
   */
  public function getCriterionId()
  {
    return $this->criterionId;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonDeviceInfo
   */
  public function setDevice(GoogleAdsSearchads360V0CommonDeviceInfo $device)
  {
    $this->device = $device;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonDeviceInfo
   */
  public function getDevice()
  {
    return $this->device;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonGenderInfo
   */
  public function setGender(GoogleAdsSearchads360V0CommonGenderInfo $gender)
  {
    $this->gender = $gender;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonGenderInfo
   */
  public function getGender()
  {
    return $this->gender;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonKeywordInfo
   */
  public function setKeyword(GoogleAdsSearchads360V0CommonKeywordInfo $keyword)
  {
    $this->keyword = $keyword;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonKeywordInfo
   */
  public function getKeyword()
  {
    return $this->keyword;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonLanguageInfo
   */
  public function setLanguage(GoogleAdsSearchads360V0CommonLanguageInfo $language)
  {
    $this->language = $language;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonLanguageInfo
   */
  public function getLanguage()
  {
    return $this->language;
  }
  /**
   * @param string
   */
  public function setLastModifiedTime($lastModifiedTime)
  {
    $this->lastModifiedTime = $lastModifiedTime;
  }
  /**
   * @return string
   */
  public function getLastModifiedTime()
  {
    return $this->lastModifiedTime;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonLocationInfo
   */
  public function setLocation(GoogleAdsSearchads360V0CommonLocationInfo $location)
  {
    $this->location = $location;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonLocationInfo
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonLocationGroupInfo
   */
  public function setLocationGroup(GoogleAdsSearchads360V0CommonLocationGroupInfo $locationGroup)
  {
    $this->locationGroup = $locationGroup;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonLocationGroupInfo
   */
  public function getLocationGroup()
  {
    return $this->locationGroup;
  }
  /**
   * @param bool
   */
  public function setNegative($negative)
  {
    $this->negative = $negative;
  }
  /**
   * @return bool
   */
  public function getNegative()
  {
    return $this->negative;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonUserListInfo
   */
  public function setUserList(GoogleAdsSearchads360V0CommonUserListInfo $userList)
  {
    $this->userList = $userList;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUserListInfo
   */
  public function getUserList()
  {
    return $this->userList;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonWebpageInfo
   */
  public function setWebpage(GoogleAdsSearchads360V0CommonWebpageInfo $webpage)
  {
    $this->webpage = $webpage;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonWebpageInfo
   */
  public function getWebpage()
  {
    return $this->webpage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCampaignCriterion::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCampaignCriterion');
