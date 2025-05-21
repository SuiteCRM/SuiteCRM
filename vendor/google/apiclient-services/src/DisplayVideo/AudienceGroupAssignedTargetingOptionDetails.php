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

namespace Google\Service\DisplayVideo;

class AudienceGroupAssignedTargetingOptionDetails extends \Google\Collection
{
  protected $collection_key = 'includedFirstPartyAndPartnerAudienceGroups';
  protected $excludedFirstPartyAndPartnerAudienceGroupType = FirstPartyAndPartnerAudienceGroup::class;
  protected $excludedFirstPartyAndPartnerAudienceGroupDataType = '';
  protected $excludedGoogleAudienceGroupType = GoogleAudienceGroup::class;
  protected $excludedGoogleAudienceGroupDataType = '';
  protected $includedCombinedAudienceGroupType = CombinedAudienceGroup::class;
  protected $includedCombinedAudienceGroupDataType = '';
  protected $includedCustomListGroupType = CustomListGroup::class;
  protected $includedCustomListGroupDataType = '';
  protected $includedFirstPartyAndPartnerAudienceGroupsType = FirstPartyAndPartnerAudienceGroup::class;
  protected $includedFirstPartyAndPartnerAudienceGroupsDataType = 'array';
  protected $includedGoogleAudienceGroupType = GoogleAudienceGroup::class;
  protected $includedGoogleAudienceGroupDataType = '';

  /**
   * @param FirstPartyAndPartnerAudienceGroup
   */
  public function setExcludedFirstPartyAndPartnerAudienceGroup(FirstPartyAndPartnerAudienceGroup $excludedFirstPartyAndPartnerAudienceGroup)
  {
    $this->excludedFirstPartyAndPartnerAudienceGroup = $excludedFirstPartyAndPartnerAudienceGroup;
  }
  /**
   * @return FirstPartyAndPartnerAudienceGroup
   */
  public function getExcludedFirstPartyAndPartnerAudienceGroup()
  {
    return $this->excludedFirstPartyAndPartnerAudienceGroup;
  }
  /**
   * @param GoogleAudienceGroup
   */
  public function setExcludedGoogleAudienceGroup(GoogleAudienceGroup $excludedGoogleAudienceGroup)
  {
    $this->excludedGoogleAudienceGroup = $excludedGoogleAudienceGroup;
  }
  /**
   * @return GoogleAudienceGroup
   */
  public function getExcludedGoogleAudienceGroup()
  {
    return $this->excludedGoogleAudienceGroup;
  }
  /**
   * @param CombinedAudienceGroup
   */
  public function setIncludedCombinedAudienceGroup(CombinedAudienceGroup $includedCombinedAudienceGroup)
  {
    $this->includedCombinedAudienceGroup = $includedCombinedAudienceGroup;
  }
  /**
   * @return CombinedAudienceGroup
   */
  public function getIncludedCombinedAudienceGroup()
  {
    return $this->includedCombinedAudienceGroup;
  }
  /**
   * @param CustomListGroup
   */
  public function setIncludedCustomListGroup(CustomListGroup $includedCustomListGroup)
  {
    $this->includedCustomListGroup = $includedCustomListGroup;
  }
  /**
   * @return CustomListGroup
   */
  public function getIncludedCustomListGroup()
  {
    return $this->includedCustomListGroup;
  }
  /**
   * @param FirstPartyAndPartnerAudienceGroup[]
   */
  public function setIncludedFirstPartyAndPartnerAudienceGroups($includedFirstPartyAndPartnerAudienceGroups)
  {
    $this->includedFirstPartyAndPartnerAudienceGroups = $includedFirstPartyAndPartnerAudienceGroups;
  }
  /**
   * @return FirstPartyAndPartnerAudienceGroup[]
   */
  public function getIncludedFirstPartyAndPartnerAudienceGroups()
  {
    return $this->includedFirstPartyAndPartnerAudienceGroups;
  }
  /**
   * @param GoogleAudienceGroup
   */
  public function setIncludedGoogleAudienceGroup(GoogleAudienceGroup $includedGoogleAudienceGroup)
  {
    $this->includedGoogleAudienceGroup = $includedGoogleAudienceGroup;
  }
  /**
   * @return GoogleAudienceGroup
   */
  public function getIncludedGoogleAudienceGroup()
  {
    return $this->includedGoogleAudienceGroup;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AudienceGroupAssignedTargetingOptionDetails::class, 'Google_Service_DisplayVideo_AudienceGroupAssignedTargetingOptionDetails');
