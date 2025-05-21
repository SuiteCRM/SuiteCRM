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

namespace Google\Service\DriveLabels;

class GoogleAppsDriveLabelsV2UpdateLabelCopyModeRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $copyMode;
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var bool
   */
  public $useAdminAccess;
  /**
   * @var string
   */
  public $view;

  /**
   * @param string
   */
  public function setCopyMode($copyMode)
  {
    $this->copyMode = $copyMode;
  }
  /**
   * @return string
   */
  public function getCopyMode()
  {
    return $this->copyMode;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param bool
   */
  public function setUseAdminAccess($useAdminAccess)
  {
    $this->useAdminAccess = $useAdminAccess;
  }
  /**
   * @return bool
   */
  public function getUseAdminAccess()
  {
    return $this->useAdminAccess;
  }
  /**
   * @param string
   */
  public function setView($view)
  {
    $this->view = $view;
  }
  /**
   * @return string
   */
  public function getView()
  {
    return $this->view;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsDriveLabelsV2UpdateLabelCopyModeRequest::class, 'Google_Service_DriveLabels_GoogleAppsDriveLabelsV2UpdateLabelCopyModeRequest');
