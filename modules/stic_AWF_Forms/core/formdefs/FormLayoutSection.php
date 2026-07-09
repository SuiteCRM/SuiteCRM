<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class FormLayoutSection {
    public FormLayout $layout;       // The layout it belongs to

    public string $id;
    public string $title;
    public string $subtitle;
    public string $containerType;    // 'panel', 'card', 'tabs', 'accordion'
    public bool $showTitle;
    public bool $isCollapsible;
    public bool $isCollapsed;
    /** @var FormLayoutElement[] */
    public array $elements = [];

    public static function fromJsonArray(FormLayout $layout, array $data): self {
        $dto = new self();

        $dto->layout = $layout;

        $dto->id = $data['id'] ?? uniqid('sect');
        $dto->title = $data['title'] ?? '';
        $dto->subtitle = $data['subtitle'] ?? '';
        $dto->showTitle = $data['showTitle'];
        $dto->isCollapsible = $data['isCollapsible'];
        $dto->isCollapsed = $data['isCollapsed'];
        $dto->containerType = $data['containerType'] ?? 'panel';
        
        if (isset($data['elements']) && is_array($data['elements'])) {
            foreach ($data['elements'] as $elData) {
                $dto->elements[] = FormLayoutElement::fromJsonArray($dto, $elData);
            }
        }

        return $dto;
    }
}