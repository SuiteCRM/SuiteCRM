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

class GoogleCloudDiscoveryengineV1alphaGetSessionRequest extends \Google\Model
{
  /**
   * @var bool
   */
  public $includeAnswerDetails;
  /**
   * @var string
   */
  public $name;

  /**
   * @param bool
   */
  public function setIncludeAnswerDetails($includeAnswerDetails)
  {
    $this->includeAnswerDetails = $includeAnswerDetails;
  }
  /**
   * @return bool
   */
  public function getIncludeAnswerDetails()
  {
    return $this->includeAnswerDetails;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaGetSessionRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaGetSessionRequest');
