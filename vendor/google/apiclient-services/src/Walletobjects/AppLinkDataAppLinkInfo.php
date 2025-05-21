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

class AppLinkDataAppLinkInfo extends \Google\Model
{
  protected $appLogoImageType = Image::class;
  protected $appLogoImageDataType = '';
  protected $appTargetType = AppLinkDataAppLinkInfoAppTarget::class;
  protected $appTargetDataType = '';
  protected $descriptionType = LocalizedString::class;
  protected $descriptionDataType = '';
  protected $titleType = LocalizedString::class;
  protected $titleDataType = '';

  /**
   * @param Image
   */
  public function setAppLogoImage(Image $appLogoImage)
  {
    $this->appLogoImage = $appLogoImage;
  }
  /**
   * @return Image
   */
  public function getAppLogoImage()
  {
    return $this->appLogoImage;
  }
  /**
   * @param AppLinkDataAppLinkInfoAppTarget
   */
  public function setAppTarget(AppLinkDataAppLinkInfoAppTarget $appTarget)
  {
    $this->appTarget = $appTarget;
  }
  /**
   * @return AppLinkDataAppLinkInfoAppTarget
   */
  public function getAppTarget()
  {
    return $this->appTarget;
  }
  /**
   * @param LocalizedString
   */
  public function setDescription(LocalizedString $description)
  {
    $this->description = $description;
  }
  /**
   * @return LocalizedString
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param LocalizedString
   */
  public function setTitle(LocalizedString $title)
  {
    $this->title = $title;
  }
  /**
   * @return LocalizedString
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AppLinkDataAppLinkInfo::class, 'Google_Service_Walletobjects_AppLinkDataAppLinkInfo');
