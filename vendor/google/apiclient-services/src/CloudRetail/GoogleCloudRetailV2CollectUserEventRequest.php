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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2CollectUserEventRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $ets;
  /**
   * @var string
   */
  public $prebuiltRule;
  /**
   * @var string
   */
  public $rawJson;
  /**
   * @var string
   */
  public $uri;
  /**
   * @var string
   */
  public $userEvent;

  /**
   * @param string
   */
  public function setEts($ets)
  {
    $this->ets = $ets;
  }
  /**
   * @return string
   */
  public function getEts()
  {
    return $this->ets;
  }
  /**
   * @param string
   */
  public function setPrebuiltRule($prebuiltRule)
  {
    $this->prebuiltRule = $prebuiltRule;
  }
  /**
   * @return string
   */
  public function getPrebuiltRule()
  {
    return $this->prebuiltRule;
  }
  /**
   * @param string
   */
  public function setRawJson($rawJson)
  {
    $this->rawJson = $rawJson;
  }
  /**
   * @return string
   */
  public function getRawJson()
  {
    return $this->rawJson;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
  /**
   * @param string
   */
  public function setUserEvent($userEvent)
  {
    $this->userEvent = $userEvent;
  }
  /**
   * @return string
   */
  public function getUserEvent()
  {
    return $this->userEvent;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2CollectUserEventRequest::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2CollectUserEventRequest');
