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

class ValueAddedModuleData extends \Google\Model
{
  protected $bodyType = LocalizedString::class;
  protected $bodyDataType = '';
  protected $headerType = LocalizedString::class;
  protected $headerDataType = '';
  protected $imageType = Image::class;
  protected $imageDataType = '';
  /**
   * @var int
   */
  public $sortIndex;
  /**
   * @var string
   */
  public $uri;
  protected $viewConstraintsType = ModuleViewConstraints::class;
  protected $viewConstraintsDataType = '';

  /**
   * @param LocalizedString
   */
  public function setBody(LocalizedString $body)
  {
    $this->body = $body;
  }
  /**
   * @return LocalizedString
   */
  public function getBody()
  {
    return $this->body;
  }
  /**
   * @param LocalizedString
   */
  public function setHeader(LocalizedString $header)
  {
    $this->header = $header;
  }
  /**
   * @return LocalizedString
   */
  public function getHeader()
  {
    return $this->header;
  }
  /**
   * @param Image
   */
  public function setImage(Image $image)
  {
    $this->image = $image;
  }
  /**
   * @return Image
   */
  public function getImage()
  {
    return $this->image;
  }
  /**
   * @param int
   */
  public function setSortIndex($sortIndex)
  {
    $this->sortIndex = $sortIndex;
  }
  /**
   * @return int
   */
  public function getSortIndex()
  {
    return $this->sortIndex;
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
   * @param ModuleViewConstraints
   */
  public function setViewConstraints(ModuleViewConstraints $viewConstraints)
  {
    $this->viewConstraints = $viewConstraints;
  }
  /**
   * @return ModuleViewConstraints
   */
  public function getViewConstraints()
  {
    return $this->viewConstraints;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ValueAddedModuleData::class, 'Google_Service_Walletobjects_ValueAddedModuleData');
