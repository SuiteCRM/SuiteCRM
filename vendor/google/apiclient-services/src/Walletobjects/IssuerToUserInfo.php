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

class IssuerToUserInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $action;
  protected $signUpInfoType = SignUpInfo::class;
  protected $signUpInfoDataType = '';
  /**
   * @var string
   */
  public $url;
  /**
   * @var string
   */
  public $value;

  /**
   * @param string
   */
  public function setAction($action)
  {
    $this->action = $action;
  }
  /**
   * @return string
   */
  public function getAction()
  {
    return $this->action;
  }
  /**
   * @param SignUpInfo
   */
  public function setSignUpInfo(SignUpInfo $signUpInfo)
  {
    $this->signUpInfo = $signUpInfo;
  }
  /**
   * @return SignUpInfo
   */
  public function getSignUpInfo()
  {
    return $this->signUpInfo;
  }
  /**
   * @param string
   */
  public function setUrl($url)
  {
    $this->url = $url;
  }
  /**
   * @return string
   */
  public function getUrl()
  {
    return $this->url;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IssuerToUserInfo::class, 'Google_Service_Walletobjects_IssuerToUserInfo');
