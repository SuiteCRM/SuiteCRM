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

class GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec extends \Google\Model
{
  /**
   * @var int
   */
  public $maxSnippetCount;
  /**
   * @var bool
   */
  public $referenceOnly;
  /**
   * @var bool
   */
  public $returnSnippet;

  /**
   * @param int
   */
  public function setMaxSnippetCount($maxSnippetCount)
  {
    $this->maxSnippetCount = $maxSnippetCount;
  }
  /**
   * @return int
   */
  public function getMaxSnippetCount()
  {
    return $this->maxSnippetCount;
  }
  /**
   * @param bool
   */
  public function setReferenceOnly($referenceOnly)
  {
    $this->referenceOnly = $referenceOnly;
  }
  /**
   * @return bool
   */
  public function getReferenceOnly()
  {
    return $this->referenceOnly;
  }
  /**
   * @param bool
   */
  public function setReturnSnippet($returnSnippet)
  {
    $this->returnSnippet = $returnSnippet;
  }
  /**
   * @return bool
   */
  public function getReturnSnippet()
  {
    return $this->returnSnippet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec');
