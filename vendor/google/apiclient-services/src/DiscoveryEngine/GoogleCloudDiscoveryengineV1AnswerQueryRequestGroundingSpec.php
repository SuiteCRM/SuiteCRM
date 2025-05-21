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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $filteringLevel;
  /**
   * @var bool
   */
  public $includeGroundingSupports;

  /**
   * @param string
   */
  public function setFilteringLevel($filteringLevel)
  {
    $this->filteringLevel = $filteringLevel;
  }
  /**
   * @return string
   */
  public function getFilteringLevel()
  {
    return $this->filteringLevel;
  }
  /**
   * @param bool
   */
  public function setIncludeGroundingSupports($includeGroundingSupports)
  {
    $this->includeGroundingSupports = $includeGroundingSupports;
  }
  /**
   * @return bool
   */
  public function getIncludeGroundingSupports()
  {
    return $this->includeGroundingSupports;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec');
