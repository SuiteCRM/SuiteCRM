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

class ActivationOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $activationUrl;
  /**
   * @var bool
   */
  public $allowReactivation;

  /**
   * @param string
   */
  public function setActivationUrl($activationUrl)
  {
    $this->activationUrl = $activationUrl;
  }
  /**
   * @return string
   */
  public function getActivationUrl()
  {
    return $this->activationUrl;
  }
  /**
   * @param bool
   */
  public function setAllowReactivation($allowReactivation)
  {
    $this->allowReactivation = $allowReactivation;
  }
  /**
   * @return bool
   */
  public function getAllowReactivation()
  {
    return $this->allowReactivation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ActivationOptions::class, 'Google_Service_Walletobjects_ActivationOptions');
